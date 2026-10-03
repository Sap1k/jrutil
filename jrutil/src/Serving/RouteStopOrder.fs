// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.Collections.Generic

/// Merge the call patterns of one route direction into a single ordered list
/// of stop slots, as printed in a line timetable. Each repeated visit of a
/// location in a pattern is its own slot, so loop lines keep both rows.
module RouteStopOrder =
    /// A distinct location sequence and how many trips run it. `times` holds
    /// the fraction (0..1) of the trip's scheduled duration at each position,
    /// or NaN where unknown; it only orders stops inserted into the same gap.
    type Pattern = {
        locations: int array
        weight: int
        times: float array
        /// Ordinal text of the sequence; breaks weight ties independently of input order.
        key: string
    }

    type private Slot(location: int) =
        let mutable timeSum = 0.0
        let mutable timeCount = 0
        member _.Location = location
        member _.Add(time: float) =
            if not (Double.IsNaN time) then
                timeSum <- timeSum + time
                timeCount <- timeCount + 1
        member _.Time = if timeCount = 0 then Double.NaN else timeSum / float timeCount

    /// Longest common subsequence of locations; returns aligned index pairs in order.
    let private align (merged: ResizeArray<Slot>) (pattern: int array) =
        let n, m = merged.Count, pattern.Length
        let table = Array2D.zeroCreate<int> (n + 1) (m + 1)
        for i in n - 1 .. -1 .. 0 do
            for j in m - 1 .. -1 .. 0 do
                table.[i, j] <-
                    if merged.[i].Location = pattern.[j] then table.[i + 1, j + 1] + 1
                    else max table.[i + 1, j] table.[i, j + 1]
        let pairs = ResizeArray<struct(int * int)>()
        let mutable i, j = 0, 0
        while i < n && j < m do
            if merged.[i].Location = pattern.[j] && table.[i, j] = table.[i + 1, j + 1] + 1 then
                pairs.Add(struct(i, j))
                i <- i + 1
                j <- j + 1
            // Prefer advancing the merged list so a pattern's stop aligns
            // with the earliest matching slot.
            elif table.[i + 1, j] >= table.[i, j + 1] then i <- i + 1
            else j <- j + 1
        pairs

    /// Interleave an existing gap with new slots, keeping each side's order.
    /// New slots go before an existing one only when both times are known and
    /// the new one is strictly earlier.
    let private interleave (existing: Slot seq) (added: Slot seq) (target: ResizeArray<Slot>) =
        let existing, added = Seq.toArray existing, Seq.toArray added
        let mutable i, j = 0, 0
        while i < existing.Length || j < added.Length do
            if j = added.Length then
                target.Add(existing.[i]); i <- i + 1
            elif i = existing.Length then
                target.Add(added.[j]); j <- j + 1
            else
                let left, right = existing.[i].Time, added.[j].Time
                if not (Double.IsNaN left) && not (Double.IsNaN right) && right < left then
                    target.Add(added.[j]); j <- j + 1
                else
                    target.Add(existing.[i]); i <- i + 1

    /// Merge patterns into ordered slots. Returns each slot's location and,
    /// per input pattern (in input order), the slot index of every position.
    let merge (patterns: Pattern array) : int array * int array array =
        let order =
            patterns
            |> Array.mapi (fun index pattern -> index, pattern)
            |> Array.sortWith (fun (_, left) (right: int * Pattern) ->
                let _, right = right
                let byWeight = compare right.weight left.weight
                if byWeight <> 0 then byWeight else
                let byLength = compare right.locations.Length left.locations.Length
                if byLength <> 0 then byLength else String.CompareOrdinal(left.key, right.key))
        let merged = ResizeArray<Slot>()
        let assigned = Array.zeroCreate<Slot array> patterns.Length
        for index, pattern in order do
            let slots = Array.zeroCreate<Slot> pattern.locations.Length
            let pairs = align merged pattern.locations
            let next = ResizeArray<Slot>(merged.Count + pattern.locations.Length)
            let mergedStart, patternStart = ref 0, ref 0
            let fillGap mergedEnd patternEnd =
                let added =
                    [| for position in patternStart.Value .. patternEnd - 1 do
                        let slot = Slot(pattern.locations.[position])
                        slot.Add(pattern.times.[position])
                        slots.[position] <- slot
                        yield slot |]
                interleave (Seq.init (mergedEnd - mergedStart.Value) (fun offset -> merged.[mergedStart.Value + offset])) added next
            for struct(mergedIndex, position) in pairs do
                fillGap mergedIndex position
                let slot = merged.[mergedIndex]
                slot.Add(pattern.times.[position])
                slots.[position] <- slot
                next.Add(slot)
                mergedStart.Value <- mergedIndex + 1
                patternStart.Value <- position + 1
            fillGap merged.Count pattern.locations.Length
            merged.Clear()
            merged.AddRange(next)
            assigned.[index] <- slots
        let positions = Dictionary<Slot, int>(HashIdentity.Reference)
        merged |> Seq.iteri (fun index slot -> positions.Add(slot, index))
        merged |> Seq.map (fun slot -> slot.Location) |> Seq.toArray,
        assigned |> Array.map (Array.map (fun slot -> positions.[slot]))
