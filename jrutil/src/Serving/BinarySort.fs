// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.IO
open System.Text
open System.Threading
open System.Collections.Generic

/// Bounded typed runs. Records stay typed through sorting and are encoded only at spill boundaries.
module BinarySort =
    let sort (root: string) (budget: int64) (maximumRows: int) (token: CancellationToken)
             (progress: string -> int64 -> unit)
             (size: 'T -> int64) (compareRows: 'T -> 'T -> int)
             (write: BinaryWriter -> 'T -> unit) (read: BinaryReader -> 'T)
             (rows: seq<'T>) = seq {
        if budget <= 0L || maximumRows <= 0 then invalidArg "budget" "Sort limits must be positive"
        let scratch = Path.Combine(root, ".binary-sort-" + Guid.NewGuid().ToString("N"))
        let comparer = Comparer<'T>.Create(Comparison<'T>(compareRows))
        let mutable next = 0
        let path () =
            next <- next + 1
            Path.Combine(scratch, string next + ".bin")
        let merge (paths: string array) = seq {
            let inputs = ResizeArray<BinaryReader>()
            try
                for path in paths do inputs.Add(new BinaryReader(File.OpenRead(path), Encoding.UTF8))
                let queue = PriorityQueue<struct(int * 'T), 'T>(comparer)
                let enqueue index =
                    let input = inputs.[index]
                    if input.BaseStream.Position < input.BaseStream.Length then
                        let row = read input
                        queue.Enqueue(struct(index, row), row)
                for index in 0 .. inputs.Count - 1 do enqueue index
                while queue.Count > 0 do
                    token.ThrowIfCancellationRequested()
                    let struct(index, row) = queue.Dequeue()
                    yield row
                    enqueue index
            finally
                for input in inputs do input.Dispose()
        }
        try
            let buffer = ResizeArray<'T>()
            let runs = ResizeArray<string>()
            let mutable bytes = 0L
            let mutable inputCount = 0L
            let flush () =
                if buffer.Count > 0 then
                    Directory.CreateDirectory(scratch) |> ignore
                    progress "sort-run" inputCount
                    buffer.Sort(comparer)
                    let target = path ()
                    use output = new BinaryWriter(File.Create(target), Encoding.UTF8)
                    for row in buffer do
                        token.ThrowIfCancellationRequested()
                        write output row
                    runs.Add(target)
                    buffer.Clear()
                    bytes <- 0L
            for row in rows do
                token.ThrowIfCancellationRequested()
                let required = size row
                if required > budget then invalidOp $"One sort record needs {required} bytes; budget is {budget}"
                if buffer.Count >= maximumRows || bytes + required > budget then flush ()
                buffer.Add(row)
                bytes <- bytes + required
                inputCount <- inputCount + 1L
                if inputCount % 16384L = 0L then progress "read-sort-input" inputCount
            if runs.Count = 0 then
                buffer.Sort(comparer)
                yield! buffer
            else
              flush ()
              let mutable current = runs.ToArray()
            // Fan-in is fixed, so reader buffers and open handles do not grow with input size.
              while current.Length > 16 do
                let reduced = ResizeArray<string>()
                for group in current |> Array.chunkBySize 16 do
                    let target = path ()
                    do
                        progress "merge-runs" 0L
                        let mutable merged = 0L
                        use output = new BinaryWriter(File.Create(target), Encoding.UTF8)
                        for row in merge group do
                            write output row
                            merged <- merged + 1L
                            if merged % 16384L = 0L then progress "merge-runs" merged
                    for source in group do File.Delete(source)
                    reduced.Add(target)
                current <- reduced.ToArray()
              progress "merge-to-parquet" 0L
              yield! merge current
        finally
            if Directory.Exists(scratch) then Directory.Delete(scratch, true)
    }
