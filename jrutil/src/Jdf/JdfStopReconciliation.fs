// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

module JrUtil.JdfStopReconciliation

open System
open System.Collections.Generic
open System.IO
open System.Text

open JrUtil.GeoData.Common
open JrUtil.JdfModel

type StopMatchKind =
    | Exact
    | Suffix
    | Fuzzy

type StopReconciliationMatch = {
    stopId: int64
    kind: StopMatchKind
    levenshteinSimilarity: float
    tokenDiceSimilarity: float
    distance: float option
}

type StopReconciliationStatistics = {
    exact: int64
    suffix: int64
    fuzzy: int64
    ambiguous: int64
    candidateComparisons: int64
    fuzzyComparisons: int64
}

type private NormalizedStopName = {
    components: string array
    tokens: string array
    flattened: string
}

type private IndexedAlias = {
    aliasId: int64
    stopId: int64
    stop: Stop
    normalized: NormalizedStopName
    location: StopLocation option
    projected: (float * float) option
}

let private synonymMap =
    lazy (
        let result = Dictionary<string, string>(StringComparer.Ordinal)
        for line in File.ReadLines(JrUtil.GeoData.StopMatcher.synonymsFile) do
            let value = line.Split('#').[0].Trim().ToLowerInvariant()
            if value <> "" then
                let alternatives =
                    value.Split(',')
                    |> Array.map (fun item -> item.Trim())
                    |> Array.filter (String.IsNullOrWhiteSpace >> not)
                let canonical =
                    alternatives
                    |> Array.sortWith (fun left right ->
                        let lengthOrder = compare right.Length left.Length
                        if lengthOrder <> 0 then lengthOrder
                        else StringComparer.Ordinal.Compare(left, right))
                    |> Array.head
                for alternative in alternatives do
                    result.[alternative] <- canonical
        result)

let private tokenize (value: string) =
    let prepared =
        value
            .Replace('\u00a0', ' ')
            .ToLowerInvariant()
            .Replace("n.l.", "nad labem")
            .Replace("n. l.", "nad labem")
            .Replace(".", ". ")
    let tokens = ResizeArray<string>()
    let token = StringBuilder()
    let flush () =
        if token.Length > 0 then
            let raw = token.ToString()
            token.Clear() |> ignore
            match synonymMap.Value.TryGetValue(raw) with
            | true, canonical -> tokens.Add(canonical)
            | false, _ -> tokens.Add(raw.TrimEnd('.'))
    for character in prepared do
        if Char.IsLetterOrDigit(character) || character = '.' then
            token.Append(character) |> ignore
        else
            flush ()
    flush ()
    tokens |> Seq.filter (String.IsNullOrWhiteSpace >> not) |> Seq.toArray

let private normalizeStopName (stop: Stop) =
    let components =
        [| Some stop.town; stop.district; stop.nearbyPlace |]
        |> Array.choose id
        |> Array.map tokenize
        |> Array.filter (Array.isEmpty >> not)
        |> Array.map (String.concat " ")
    let tokens = components |> Array.collect (fun namePart -> namePart.Split(' '))
    {
        components = components
        tokens = tokens
        flattened = String.concat " " tokens
    }

let private componentKey (components: string array) =
    String.concat "\u001f" components

let private rawAliasKey (stop: Stop) =
    String.concat "\u001f" [|
        stop.town.Trim().ToLowerInvariant()
        stop.district |> Option.defaultValue "" |> fun value -> value.Trim().ToLowerInvariant()
        stop.nearbyPlace |> Option.defaultValue "" |> fun value -> value.Trim().ToLowerInvariant()
    |]

let private compatibleOptional left right =
    match left, right with
    | Some left, Some right -> StringComparer.OrdinalIgnoreCase.Equals(left, right)
    | _ -> true

let private compatibleLocality (left: Stop) (right: Stop) =
    compatibleOptional left.country right.country
    && compatibleOptional left.regionId right.regionId

let private isSuffix (shorter: string array) (longer: string array) =
    shorter.Length < longer.Length
    && Array.forall2
        (=)
        shorter
        longer.[longer.Length - shorter.Length..]

let private tokenDice (left: string array) (right: string array) =
    let leftSet = Set.ofArray left
    let rightSet = Set.ofArray right
    if Set.isEmpty leftSet && Set.isEmpty rightSet then 1.0
    else
        2.0 * float (Set.intersect leftSet rightSet).Count
        / float (leftSet.Count + rightSet.Count)

let private levenshteinSimilarity (left: string) (right: string) =
    if left = right then 1.0
    elif left.Length = 0 || right.Length = 0 then 0.0
    else
        let previous = Array.init (right.Length + 1) id
        let current = Array.zeroCreate<int> (right.Length + 1)
        for leftIndex in 1..left.Length do
            current.[0] <- leftIndex
            for rightIndex in 1..right.Length do
                let substitution =
                    previous.[rightIndex - 1]
                    + if left.[leftIndex - 1] = right.[rightIndex - 1] then 0 else 1
                current.[rightIndex] <-
                    min
                        (min (previous.[rightIndex] + 1) (current.[rightIndex - 1] + 1))
                        substitution
            Array.Copy(current, previous, current.Length)
        1.0 - float previous.[right.Length] / float (max left.Length right.Length)

let private preciseProjected (location: StopLocation option) =
    location
    |> Option.filter (fun value -> value.precision = StopPrecise)
    |> Option.map (fun value ->
        let point =
            wgs84Factory.CreatePoint(
                NetTopologySuite.Geometries.Coordinate(float value.lon, float value.lat))
            |> pointWgs84ToEtrs89Ex
        point.X, point.Y)

let private distance left right =
    let dx = fst left - fst right
    let dy = snd left - snd right
    sqrt (dx * dx + dy * dy)

let private addIndexValue (index: Dictionary<'key, HashSet<int64>>) key value =
    match index.TryGetValue(key) with
    | true, values -> values.Add(value) |> ignore
    | false, _ -> index.[key] <- HashSet([value])

/// Incremental, indexed reconciliation for the name-based JDF merge strategy.
/// Name candidates use dictionaries; fuzzy candidates are limited to adjacent
/// projected 75 m grid cells.
type StopReconciler() =
    let distanceThreshold = 75.0
    let exactIndex = Dictionary<string, HashSet<int64>>(StringComparer.Ordinal)
    let suffixIndex = Dictionary<string, HashSet<int64>>(StringComparer.Ordinal)
    let spatialIndex = Dictionary<struct (int * int), HashSet<int64>>()
    let aliases = Dictionary<int64, IndexedAlias>()
    let aliasIdsByKey = Dictionary<struct (int64 * string), int64>()
    let mutable nextAliasId = 0L
    let mutable exactCount = 0L
    let mutable suffixCount = 0L
    let mutable fuzzyCount = 0L
    let mutable ambiguousCount = 0L
    let mutable candidateComparisons = 0L
    let mutable fuzzyComparisons = 0L

    let gridCell (x, y) =
        struct (int (floor (x / distanceThreshold)), int (floor (y / distanceThreshold)))

    let indexedIds (index: Dictionary<string, HashSet<int64>>) (key: string) =
        match index.TryGetValue(key) with
        | true, values -> values :> seq<int64>
        | false, _ -> Seq.empty

    let nameCandidateIds normalized =
        seq {
            let fullKey = componentKey normalized.components
            yield! indexedIds exactIndex fullKey
            if normalized.components.Length >= 2 then
                yield! indexedIds suffixIndex fullKey
            for offset in 1..normalized.components.Length - 1 do
                let suffixKey = componentKey normalized.components.[offset..]
                yield! indexedIds exactIndex suffixKey
        }
        |> set

    let nearbyCandidateIds projected =
        match projected with
        | None -> Set.empty
        | Some point ->
            let struct (cellX, cellY) = gridCell point
            let spatialCandidates =
                seq {
                    for xOffset in -1..1 do
                        for yOffset in -1..1 do
                            let key = struct (cellX + xOffset, cellY + yOffset)
                            match spatialIndex.TryGetValue(key) with
                            | true, values -> yield! values
                            | false, _ -> ()
                }
                |> set
            spatialCandidates

    let exactOrSuffix normalized alias =
        if normalized.components = alias.normalized.components then Some Exact
        elif isSuffix normalized.components alias.normalized.components
             || isSuffix alias.normalized.components normalized.components then Some Suffix
        else None

    let matchAlias stop normalized projected alias =
        candidateComparisons <- candidateComparisons + 1L
        if not (compatibleLocality stop alias.stop) then None
        else
            let dist =
                match projected, alias.projected with
                | Some left, Some right -> Some (distance left right)
                | _ -> None
            match exactOrSuffix normalized alias with
            | Some kind when dist |> Option.exists (fun value -> value > distanceThreshold) -> None
            | Some kind ->
                Some {
                    stopId = alias.stopId
                    kind = kind
                    levenshteinSimilarity = levenshteinSimilarity normalized.flattened alias.normalized.flattened
                    tokenDiceSimilarity = tokenDice normalized.tokens alias.normalized.tokens
                    distance = dist
                }
            | None ->
                match dist with
                | Some value when value <= distanceThreshold
                                  && normalized.tokens.Length >= 2
                                  && alias.normalized.tokens.Length >= 2 ->
                    fuzzyComparisons <- fuzzyComparisons + 1L
                    let levenshtein =
                        levenshteinSimilarity normalized.flattened alias.normalized.flattened
                    let dice = tokenDice normalized.tokens alias.normalized.tokens
                    if levenshtein >= 0.90 && dice >= 0.80 then
                        Some {
                            stopId = alias.stopId
                            kind = Fuzzy
                            levenshteinSimilarity = levenshtein
                            tokenDiceSimilarity = dice
                            distance = Some value
                        }
                    else None
                | _ -> None

    member _.FindMatch(stop: Stop, location: StopLocation option) =
        let normalized = normalizeStopName stop
        let projected = preciseProjected location
        let candidateIds =
            Set.union (nameCandidateIds normalized) (nearbyCandidateIds projected)
        let allMatches =
            candidateIds
            |> Seq.choose (fun aliasId -> matchAlias stop normalized projected aliases.[aliasId])
            |> Seq.groupBy (fun candidate -> candidate.stopId)
            |> Seq.map (fun (_, candidates) ->
                candidates
                |> Seq.sortBy (fun candidate ->
                    (match candidate.kind with Exact -> 0 | Suffix -> 1 | Fuzzy -> 2),
                    (candidate.distance |> Option.defaultValue 0.0))
                |> Seq.head)
            |> Seq.toArray
        let matches =
            match allMatches with
            | [||] -> [||]
            | candidates ->
                let rank = function Exact -> 0 | Suffix -> 1 | Fuzzy -> 2
                let strongestRank = candidates |> Array.map (fun candidate -> rank candidate.kind) |> Array.min
                candidates |> Array.filter (fun candidate -> rank candidate.kind = strongestRank)
        match matches with
        | [| candidate |] ->
            match candidate.kind with
            | Exact -> exactCount <- exactCount + 1L
            | Suffix -> suffixCount <- suffixCount + 1L
            | Fuzzy -> fuzzyCount <- fuzzyCount + 1L
            Choice1Of2 candidate
        | [||] -> Choice2Of2 [||]
        | candidates ->
            ambiguousCount <- ambiguousCount + 1L
            Choice2Of2 candidates

    member _.AddAlias(stopId: int64, stop: Stop, location: StopLocation option) =
        let aliasKey = struct (stopId, rawAliasKey stop)
        match aliasIdsByKey.TryGetValue(aliasKey) with
        | true, aliasId ->
            let existing = aliases.[aliasId]
            let updatedLocation =
                match existing.location, location with
                | None, Some incoming -> Some incoming
                | Some current, Some incoming
                    when current.precision <> StopPrecise
                         && incoming.precision = StopPrecise -> Some incoming
                | _ -> existing.location
            let updatedProjected = preciseProjected updatedLocation
            if updatedProjected <> existing.projected then
                existing.projected
                |> Option.iter (fun point ->
                    let values = spatialIndex.[gridCell point]
                    values.Remove(aliasId) |> ignore
                    if values.Count = 0 then
                        spatialIndex.Remove(gridCell point) |> ignore)
                updatedProjected
                |> Option.iter (fun point ->
                    addIndexValue spatialIndex (gridCell point) aliasId)
            aliases.[aliasId] <- {
                existing with
                    stop = {
                        existing.stop with
                            regionId = existing.stop.regionId |> Option.orElse stop.regionId
                            country = existing.stop.country |> Option.orElse stop.country
                    }
                    location = updatedLocation
                    projected = updatedProjected
            }
        | false, _ ->
            nextAliasId <- nextAliasId + 1L
            let normalized = normalizeStopName stop
            let projected = preciseProjected location
            let alias = {
                aliasId = nextAliasId
                stopId = stopId
                stop = stop
                normalized = normalized
                location = location
                projected = projected
            }
            aliases.[alias.aliasId] <- alias
            aliasIdsByKey.[aliasKey] <- alias.aliasId
            addIndexValue exactIndex (componentKey normalized.components) alias.aliasId
            for offset in 1..normalized.components.Length - 1 do
                let suffix = normalized.components.[offset..]
                if suffix.Length >= 2
                   && (suffix |> Array.sumBy (fun value -> value.Split(' ').Length)) >= 2 then
                    addIndexValue suffixIndex (componentKey suffix) alias.aliasId
            projected
            |> Option.iter (fun point ->
                addIndexValue spatialIndex (gridCell point) alias.aliasId)

    member _.Statistics = {
        exact = exactCount
        suffix = suffixCount
        fuzzy = fuzzyCount
        ambiguous = ambiguousCount
        candidateComparisons = candidateComparisons
        fuzzyComparisons = fuzzyComparisons
    }

let stopDisplayName (stop: Stop) =
    [| Some stop.town; stop.district; stop.nearbyPlace |]
    |> Array.choose id
    |> String.concat ","

let isCanonicalNamePreferred (candidate: Stop) (current: Stop) =
    let rank (stop: Stop) =
        let components =
            [| Some stop.town; stop.district; stop.nearbyPlace |]
            |> Array.choose id
            |> Array.filter (String.IsNullOrWhiteSpace >> not)
        let display = stopDisplayName stop
        let redundantAdjacentComponents =
            components
            |> Array.pairwise
            |> Array.filter (fun (left, right) ->
                StringComparer.OrdinalIgnoreCase.Equals(left.Trim(), right.Trim()))
            |> Array.length
        -redundantAdjacentComponents,
        components.Length,
        -(display |> Seq.filter ((=) '.') |> Seq.length),
        display.Length
    let candidateRank = rank candidate
    let currentRank = rank current
    candidateRank > currentRank
    || (candidateRank = currentRank
        && StringComparer.Ordinal.Compare(stopDisplayName candidate, stopDisplayName current) < 0)
