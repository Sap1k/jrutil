// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.Collections.Generic
open System.IO
open System.IO.Hashing
open System.Text
open System.Threading

/// Primary-key deduplication without ordering. Identical rows collapse and a
/// key with differing rows is a compiler error, exactly as the former sorted
/// writers enforced, but no comparison sort or run merge takes place.
///
/// Rows are kept typed in memory while they fit `budget`; beyond that they are
/// spilled once into 256 partitions by key hash and each partition is
/// deduplicated on its own, so memory stays bounded. Output order is the input
/// order (in memory) or partition-then-input order (spilled): deterministic
/// for a deterministic input, but not sorted.
module HashDedup =
    let private hashText (text: string) = XxHash128.HashToUInt128(Encoding.UTF8.GetBytes(text).AsSpan())

    let dedup (root: string) (budget: int64) (token: CancellationToken) (progress: string -> int64 -> unit)
              (relation: string) (size: 'T -> int64) (key: 'T -> string)
              (write: BinaryWriter -> 'T -> unit) (read: BinaryReader -> 'T)
              (rows: seq<'T>) = seq {
        if budget <= 0L then invalidArg "budget" "Deduplication budget must be positive"
        let encoded = new MemoryStream()
        let encoder = new BinaryWriter(encoded, Encoding.UTF8, true)
        let rowHash row =
            encoded.SetLength(0L)
            write encoder row
            encoder.Flush()
            XxHash128.HashToUInt128(ReadOnlySpan(encoded.GetBuffer(), 0, int encoded.Length))
        /// Returns true when `row` is new; fails on a conflicting row for a seen key.
        let admit (seen: Dictionary<UInt128, UInt128>) (row: 'T) (keyHash: UInt128) =
            let hash = rowHash row
            match seen.TryGetValue(keyHash) with
            | true, previous when previous = hash -> false
            | true, _ -> invalidOp $"Relation {relation} contains conflicting rows for primary key {key row}"
            | _ -> seen.[keyHash] <- hash; true
        let scratch = Path.Combine(root, ".hash-dedup-" + Guid.NewGuid().ToString("N"))
        try
            let seen = Dictionary<UInt128, UInt128>()
            let buffer = ResizeArray<'T>()
            let mutable bytes = 0L
            let mutable count = 0L
            let mutable partitions: BinaryWriter array = null
            let spill (row: 'T) (keyHash: UInt128) =
                write partitions.[int (keyHash >>> 120)] row
            for row in rows do
                token.ThrowIfCancellationRequested()
                count <- count + 1L
                if count % 65536L = 0L then progress "dedup-input" count
                let keyHash = hashText (key row)
                if isNull partitions then
                    if admit seen row keyHash then
                        buffer.Add(row)
                        bytes <- bytes + size row
                        if bytes > budget then
                            Directory.CreateDirectory(scratch) |> ignore
                            partitions <- Array.init 256 (fun index ->
                                new BinaryWriter(File.Create(Path.Combine(scratch, $"{index:x2}.bin")), Encoding.UTF8))
                            for buffered in buffer do spill buffered (hashText (key buffered))
                            buffer.Clear()
                            seen.Clear()
                            seen.TrimExcess()
                else spill row keyHash
            if isNull partitions then
                yield! buffer
            else
                for writer in partitions do writer.Dispose()
                for index in 0 .. 255 do
                    progress "dedup-partition" (int64 index)
                    let partitionSeen = Dictionary<UInt128, UInt128>()
                    use input = new BinaryReader(File.OpenRead(Path.Combine(scratch, $"{index:x2}.bin")), Encoding.UTF8)
                    while input.BaseStream.Position < input.BaseStream.Length do
                        token.ThrowIfCancellationRequested()
                        let row = read input
                        if admit partitionSeen row (hashText (key row)) then yield row
        finally
            encoder.Dispose()
            encoded.Dispose()
            if Directory.Exists(scratch) then Directory.Delete(scratch, true)
    }
