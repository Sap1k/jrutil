// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.IO
open System.Threading

/// Shared bounded typed deduplication and Parquet finalization for compiler relations.
module RelationWriter =
    /// Write rows in the given order, flushing bounded column batches.
    let writeRows (path: string) (schema: Schema.Relation)
                  columnBudget maximumColumnRows (token: CancellationToken) progress
                  size columns rows =
        use output = new ColumnWriter.Writer(path, schema, maximumColumnRows, token)
        let buffer = ResizeArray<_>()
        let mutable bufferBytes = 0L
        let flush () =
            if buffer.Count > 0 then
                output.Append(columns (buffer.ToArray()))
                buffer.Clear()
                bufferBytes <- 0L
                progress "write-parquet" output.RowCount
        for row in rows do
            token.ThrowIfCancellationRequested()
            let required = size row
            if required > columnBudget then invalidOp $"One {schema.name} record needs {required} bytes; column budget is {columnBudget}"
            if buffer.Count = maximumColumnRows || bufferBytes + required > columnBudget then flush ()
            buffer.Add(row)
            bufferBytes <- bufferBytes + required
        flush ()
        int output.RowCount

    /// Deduplicate by primary key (`key` renders it as text) and write.
    let write (path: string) (schema: Schema.Relation) dedupBudget
              columnBudget maximumColumnRows (token: CancellationToken) progress
              size key encode decode columns rows =
        let unique = HashDedup.dedup (Path.GetDirectoryName(path)) dedupBudget token progress schema.name size key encode decode rows
        writeRows path schema columnBudget maximumColumnRows token progress size columns unique

    /// Concatenate relation files of one schema (rows in file order).
    let concat (path: string) (schema: Schema.Relation) (parts: string seq) (token: CancellationToken) =
        use output = new ColumnWriter.Writer(path, schema, 8192, token, 64L * 1024L * 1024L)
        for part in parts do
            for columns in ColumnReader.groups part schema do
                let rows = if columns.Length = 0 then 0 else ColumnWriter.rowCount columns.[0]
                for start in 0 .. 8192 .. rows - 1 do
                    output.Append(ColumnReader.slice start (min 8192 (rows - start)) columns)
        int output.RowCount
