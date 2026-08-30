// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
module internal JrUtil.RegionalOverlay.DateSet

open System
open System.Collections
open System.Collections.Generic
open System.Security.Cryptography

/// Immutable packed calendar. Bit order is the historical date-key encoding.
type Dates private (length: int, bytes: byte array) =
    let key = lazy (Convert.ToHexString(SHA256.HashData(bytes)).ToLowerInvariant())
    member private _.PackedBytes = bytes
    member _.Length = length
    member _.Any = bytes |> Array.exists ((<>) 0uy)
    member _.Key = key.Value
    member _.Item with get index =
        if index < 0 || index >= length then invalidArg "index" "Date index outside window"
        bytes.[index / 8] &&& (1uy <<< (index % 8)) <> 0uy
    member _.Count = bytes |> Array.sumBy (fun value -> int (System.Numerics.BitOperations.PopCount(uint32 value)))
    member _.Overlaps(other: Dates) =
        if length <> other.Length then invalidArg "other" "Date windows differ"
        Array.exists2 (fun left right -> left &&& right <> 0uy) bytes other.PackedBytes
    member _.Intersect(other: Dates) =
        if length <> other.Length then invalidArg "other" "Date windows differ"
        Dates(length, Array.map2 (&&&) bytes other.PackedBytes)
    static member Of(values: seq<bool>) =
        match values with
        | :? Dates as dates -> dates
        | _ ->
            let values = Seq.toArray values
            let bytes = Array.zeroCreate<byte> ((values.Length + 7) / 8)
            for i in 0 .. values.Length - 1 do
                if values.[i] then bytes.[i / 8] <- bytes.[i / 8] ||| (1uy <<< (i % 8))
            Dates(values.Length, bytes)
    interface IEnumerable<bool> with
        member this.GetEnumerator() = (seq { for i in 0 .. length - 1 -> this.[i] }).GetEnumerator()
    interface IEnumerable with
        member this.GetEnumerator() = (this :> IEnumerable<bool>).GetEnumerator() :> IEnumerator

/// Intern sets by the full canonical digest within the owning stage.
type Pool() =
    let values = Dictionary<string, Dates>(StringComparer.Ordinal)
    member _.Intern(dates: seq<bool>) =
        let packed = match dates with | :? Dates as packed -> packed | _ -> Dates.Of dates
        match values.TryGetValue(packed.Key) with
        | true, existing when existing.Length = packed.Length -> existing
        | _ -> values.[packed.Key] <- packed; packed
