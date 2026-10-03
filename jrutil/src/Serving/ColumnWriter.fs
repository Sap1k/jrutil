// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.Collections.Generic
open System.IO
open System.Threading
open Parquet
open Parquet.Schema

/// Typed column batches are the serialization boundary. Producers own their
/// bounded buffers; writing a batch is synchronous and never retains a batch.
module ColumnWriter =
    type Column =
        | Text of string array
        | Int16 of int16 array
        | OptionalInt16 of Nullable<int16> array
        | Int32 of int array
        | OptionalInt32 of Nullable<int> array
        | Int64 of int64 array
        | OptionalInt64 of Nullable<int64> array
        | Float64 of double array
        | OptionalFloat64 of Nullable<double> array
        | Boolean of bool array
        | OptionalBoolean of Nullable<bool> array
        | Date of DateOnly array
        | OptionalDate of Nullable<DateOnly> array

    let private dataField (field: JrUtil.Serving.Schema.Field) : DataField =
        match field.dataType with
        | Schema.Text -> DataField<string>(field.name, Nullable field.nullable)
        | Schema.Int16 -> DataField<int16>(field.name, Nullable field.nullable)
        | Schema.Int32 -> DataField<int>(field.name, Nullable field.nullable)
        | Schema.Int64 -> DataField<int64>(field.name, Nullable field.nullable)
        | Schema.Float64 -> DataField<double>(field.name, Nullable field.nullable)
        | Schema.Boolean -> DataField<bool>(field.name, Nullable field.nullable)
        | Schema.Date -> DataField<DateOnly>(field.name, Nullable field.nullable)

    let private describe = function
        | Text values -> Schema.Text, false, values.Length
        | Int16 values -> Schema.Int16, false, values.Length
        | OptionalInt16 values -> Schema.Int16, true, values.Length
        | Int32 values -> Schema.Int32, false, values.Length
        | OptionalInt32 values -> Schema.Int32, true, values.Length
        | Int64 values -> Schema.Int64, false, values.Length
        | OptionalInt64 values -> Schema.Int64, true, values.Length
        | Float64 values -> Schema.Float64, false, values.Length
        | OptionalFloat64 values -> Schema.Float64, true, values.Length
        | Boolean values -> Schema.Boolean, false, values.Length
        | OptionalBoolean values -> Schema.Boolean, true, values.Length
        | Date values -> Schema.Date, false, values.Length
        | OptionalDate values -> Schema.Date, true, values.Length

    // Includes array headers and text objects, even when a producer happens to
    // share strings. Compression/native headroom belongs to the execution budget.
    let estimatedBytes column =
        let _, _, count = describe column
        let width =
            match column with
            | Text _ -> 8L
            | Boolean _ -> 1L
            | Int16 _ | OptionalBoolean _ -> 2L
            | Int32 _ | Date _ | OptionalInt16 _ -> 4L
            | Int64 _ | Float64 _ | OptionalInt32 _ | OptionalDate _ -> 8L
            | OptionalInt64 _ | OptionalFloat64 _ -> 16L
        let strings =
            match column with
            | Text values -> values |> Array.sumBy (fun value -> if isNull value then 0L else 24L + 2L * int64 value.Length)
            | _ -> 0L
        24L + width * int64 count + strings

    let private writeValues<'T when 'T : (new : unit -> 'T) and 'T : struct and 'T :> ValueType>
            (group: ParquetRowGroupWriter) field token (values: 'T array) =
        group.WriteAsync<'T>(field, ReadOnlyMemory<'T>(values), Nullable<ReadOnlyMemory<int>>(), null, token).GetAwaiter().GetResult()

    let private writeOptional<'T when 'T : (new : unit -> 'T) and 'T : struct and 'T :> ValueType>
            (group: ParquetRowGroupWriter) field token (values: Nullable<'T> array) =
        group.WriteAsync<'T>(field, ReadOnlyMemory<Nullable<'T>>(values), Nullable<ReadOnlyMemory<int>>(), null, token).GetAwaiter().GetResult()

    type Writer(path: string, relation: JrUtil.Serving.Schema.Relation, maximumRows: int, token: CancellationToken,
                ?maximumBytes: int64) =
        let maximumBytes = defaultArg maximumBytes (16L * 1024L * 1024L)
        let fields = relation.fields |> Array.map dataField
        let stream =
            if maximumRows <= 0 then invalidArg "maximumRows" "Row group limit must be positive"
            if maximumBytes <= 0L then invalidArg "maximumBytes" "Column byte limit must be positive"
            token.ThrowIfCancellationRequested()
            File.Open(path, FileMode.CreateNew, FileAccess.Write, FileShare.None)
        let writer =
            try
                ParquetWriter.CreateAsync(ParquetSchema(fields |> Array.map (fun field -> field :> Field)),
                    stream, ParquetOptions(CompressionMethod = CompressionMethod.Snappy,
                                           MaximumSmallPoolFreeBytes = 1024 * 1024,
                                           MaximumLargePoolFreeBytes = 4 * 1024 * 1024), false, token).GetAwaiter().GetResult()
            with _ -> stream.Dispose(); reraise()
        let mutable count = 0L
        let mutable disposed = false
        do
            writer.CustomMetadata <- Dictionary<string, string>(dict [
                "obehy.schema_version", string Schema.ServingSchemaVersion
                "obehy.relation", relation.name
                "jrutil.bundle_format", Schema.BundleFormat ])

        member _.RowCount = count

        member _.Append(columns: Column array) =
            if disposed then raise (ObjectDisposedException("ColumnWriter"))
            token.ThrowIfCancellationRequested()
            if columns.Length <> fields.Length then invalidArg "columns" "Column count differs from relation schema"
            let _, _, rows = describe columns.[0]
            if rows > maximumRows then invalidArg "columns" "Column batch exceeds row group limit"
            for index in 0 .. columns.Length - 1 do
                let actualType, nullable, length = describe columns.[index]
                let expected = relation.fields.[index]
                if length <> rows || actualType <> expected.dataType
                   || (actualType <> Schema.Text && nullable <> expected.nullable) then
                    invalidArg "columns" $"Invalid column batch for {relation.name}.{expected.name}"
                match columns.[index] with
                | Text values when not expected.nullable && Array.exists isNull values ->
                    invalidArg "columns" $"Null in required column {relation.name}.{expected.name}"
                | _ -> ()
            let bytes = columns |> Array.sumBy estimatedBytes
            if bytes > maximumBytes then
                invalidArg "columns" $"{relation.name}: column batch needs {bytes} bytes; limit is {maximumBytes}. Split the batch before writing."
            if rows > 0 then
                use group = writer.CreateRowGroup()
                for index in 0 .. columns.Length - 1 do
                    token.ThrowIfCancellationRequested()
                    let field = fields.[index]
                    match columns.[index] with
                    | Text values -> group.WriteAsync(field, values :> IReadOnlyCollection<string>, Nullable<ReadOnlyMemory<int>>()).GetAwaiter().GetResult()
                    | Int16 values -> writeValues group field token values
                    | OptionalInt16 values -> writeOptional group field token values
                    | Int32 values -> writeValues group field token values
                    | OptionalInt32 values -> writeOptional group field token values
                    | Int64 values -> writeValues group field token values
                    | OptionalInt64 values -> writeOptional group field token values
                    | Float64 values -> writeValues group field token values
                    | OptionalFloat64 values -> writeOptional group field token values
                    | Boolean values -> writeValues group field token values
                    | OptionalBoolean values -> writeOptional group field token values
                    | Date values -> writeValues group field token values
                    | OptionalDate values -> writeOptional group field token values
                count <- count + int64 rows

        interface IDisposable with
            member _.Dispose() =
                if not disposed then
                    disposed <- true
                    try writer.DisposeAsync().AsTask().GetAwaiter().GetResult()
                    finally stream.Dispose()

/// Typed row-group reads of a relation written by ColumnWriter, in schema
/// field order. Used to rewrite finalized relations without materializing them.
module ColumnReader =
    open ColumnWriter

    let private values<'T when 'T : (new : unit -> 'T) and 'T : struct and 'T :> ValueType>
            (group: ParquetRowGroupReader) field count =
        let result = Array.zeroCreate<'T> count
        group.ReadAsync<'T>(field, result.AsMemory(), Nullable(), CancellationToken.None).AsTask().GetAwaiter().GetResult()
        result

    let private optionalValues<'T when 'T : (new : unit -> 'T) and 'T : struct and 'T :> ValueType>
            (group: ParquetRowGroupReader) field count =
        let result = Array.zeroCreate<Nullable<'T>> count
        group.ReadAsync<'T>(field, result.AsMemory(), Nullable(), CancellationToken.None).AsTask().GetAwaiter().GetResult()
        result

    let private column (group: ParquetRowGroupReader) (field: DataField) (expected: JrUtil.Serving.Schema.Field) count =
        match expected.dataType, expected.nullable with
        | Schema.Text, _ ->
            let result = Array.zeroCreate<string> count
            group.ReadAsync(field, result.AsMemory(), Nullable(), CancellationToken.None).AsTask().GetAwaiter().GetResult()
            Text result
        | Schema.Int16, false -> Int16 (values<int16> group field count)
        | Schema.Int16, true -> OptionalInt16 (optionalValues<int16> group field count)
        | Schema.Int32, false -> Int32 (values<int> group field count)
        | Schema.Int32, true -> OptionalInt32 (optionalValues<int> group field count)
        | Schema.Int64, false -> Int64 (values<int64> group field count)
        | Schema.Int64, true -> OptionalInt64 (optionalValues<int64> group field count)
        | Schema.Float64, false -> Float64 (values<double> group field count)
        | Schema.Float64, true -> OptionalFloat64 (optionalValues<double> group field count)
        | Schema.Boolean, false -> Boolean (values<bool> group field count)
        | Schema.Boolean, true -> OptionalBoolean (optionalValues<bool> group field count)
        | Schema.Date, false -> Date (values<DateOnly> group field count)
        | Schema.Date, true -> OptionalDate (optionalValues<DateOnly> group field count)

    /// Row groups of `path` as typed column batches in schema field order.
    let groups (path: string) (relation: JrUtil.Serving.Schema.Relation) = seq {
        use stream = File.OpenRead(path)
        let reader = ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
        try
            let fields = reader.Schema.DataFields
            if fields.Length <> relation.fields.Length then
                invalidOp $"{relation.name}: expected {relation.fields.Length} fields, found {fields.Length}"
            for index in 0 .. reader.RowGroupCount - 1 do
                use group = reader.OpenRowGroupReader(index)
                let count = int group.RowCount
                yield Array.map2 (fun field expected -> column group field expected count) fields relation.fields
        finally
            reader.DisposeAsync().AsTask().GetAwaiter().GetResult()
    }

    /// Rows `start .. start + count - 1` of a column batch.
    let slice start count (columns: Column array) =
        let take (values: 'T array) = Array.sub values start count
        columns |> Array.map (function
            | Text values -> Text (take values)
            | Int16 values -> Int16 (take values)
            | OptionalInt16 values -> OptionalInt16 (take values)
            | Int32 values -> Int32 (take values)
            | OptionalInt32 values -> OptionalInt32 (take values)
            | Int64 values -> Int64 (take values)
            | OptionalInt64 values -> OptionalInt64 (take values)
            | Float64 values -> Float64 (take values)
            | OptionalFloat64 values -> OptionalFloat64 (take values)
            | Boolean values -> Boolean (take values)
            | OptionalBoolean values -> OptionalBoolean (take values)
            | Date values -> Date (take values)
            | OptionalDate values -> OptionalDate (take values))

    let text = function Text values -> values | _ -> invalidOp "Expected a text column"
    let int32 = function Int32 values -> values | _ -> invalidOp "Expected a required int32 column"
    let optionalInt16 = function OptionalInt16 values -> values | _ -> invalidOp "Expected a nullable int16 column"
    let optionalInt32 = function OptionalInt32 values -> values | _ -> invalidOp "Expected a nullable int32 column"
