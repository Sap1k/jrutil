// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.IO
open System.Threading

/// Shared bounded typed sort/merge and Parquet finalization for compiler relations.
module RelationWriter =
    let writeOrdered (path: string) (schema: Schema.Relation)
                     columnBudget maximumColumnRows (token: CancellationToken) progress
                     size compareRows equivalent columns rows =
        use output = new ColumnWriter.Writer(path, schema, maximumColumnRows, token)
        let buffer = ResizeArray<_>()
        let mutable bufferBytes = 0L
        let mutable previous = None
        let flush () =
            if buffer.Count > 0 then
                output.Append(columns (buffer.ToArray()))
                buffer.Clear()
                bufferBytes <- 0L
                progress "merge-to-parquet" output.RowCount
        for row in rows do
            token.ThrowIfCancellationRequested()
            match previous with
            | Some prior when compareRows prior row > 0 ->
                invalidOp $"Rows are not ordered by the primary key in {schema.name}"
            | Some prior when compareRows prior row = 0 ->
                if not (equivalent prior row) then invalidOp $"Conflicting rows for a primary key in {schema.name}"
            | _ ->
                let required = size row
                if required > columnBudget then invalidOp $"One {schema.name} record needs {required} bytes; column budget is {columnBudget}"
                if buffer.Count = maximumColumnRows || bufferBytes + required > columnBudget then flush ()
                buffer.Add(row)
                bufferBytes <- bufferBytes + required
                previous <- Some row
        flush ()
        int output.RowCount

    let write (path: string) (schema: Schema.Relation) sortBudget maximumSortRows
              columnBudget maximumColumnRows (token: CancellationToken) progress
              size compareRows equivalent encode decode columns rows =
        let sorted = BinarySort.sort (Path.GetDirectoryName(path)) sortBudget maximumSortRows token progress size compareRows encode decode rows
        writeOrdered path schema columnBudget maximumColumnRows token progress size compareRows equivalent columns sorted
