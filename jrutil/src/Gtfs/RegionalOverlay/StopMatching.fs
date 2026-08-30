// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.StopMatching

open System
open System.Collections.Generic
open System.IO
open NodaTime
open Serilog

open JrUtil.GtfsModel

open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Runtime
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay

type Input = {
    prepared: InputPreparation.Result
    sourceStopRows: CsvRow array
    sourceStopGroups: StopGroup array
    sourceGroupByMember: Dictionary<string, string>
}

type Result = {
    stopGroupMatches: Dictionary<string, string>
    stopMatchMethods: Dictionary<string, string>
    stopMatchDistances: Dictionary<string, float>
    mappedPlaceBySourceStop: Map<string, string>
}

/// Resolve initial stop groups using reviewed overrides, names and spatial evidence.
let matchStops ({
    prepared = prepared
    sourceStopRows = sourceStopRows
    sourceStopGroups = sourceStopGroups
    sourceGroupByMember = sourceGroupByMember
}: Input) : Result =
    let stopOverrideBySource = prepared.stopOverrides |> Array.groupBy (fun value -> value.sourceId) |> dict
    let resolveStopOverride (value: OverrideBinding) =
        match value.targetNamespace with
        | "jdf-stop-place-name" ->
            let key = prepared.stableStopNameKey value.targetId
            match prepared.baseStopGroupsByName.TryGetValue(key) with
            | true, targets when targets.Length = 1 -> [| targets.[0].groupId |]
            | true, targets ->
                addDiagnostic prepared.diagnostics "stop_override_target_ambiguous" value.sourceId $"Stable target name {value.targetId} resolves to {targets.Length} base stop places"
                [||]
            | _ ->
                addDiagnostic prepared.diagnostics "stop_override_target_missing" value.sourceId $"Stable target name not found: {value.targetId}"
                [||]
        | "jdf-stop-place" -> [| value.targetId |]
        | namespaceName ->
            addDiagnostic prepared.diagnostics "stop_override_namespace_unsupported" value.sourceId namespaceName
            [||]
    let stopGroupMatches = Dictionary<string, string>(StringComparer.Ordinal)
    let stopMatchMethods = Dictionary<string, string>(StringComparer.Ordinal)
    let stopMatchDistances = Dictionary<string, float>(StringComparer.Ordinal)
    for sourceGroup in sourceStopGroups do
        let overrideTarget =
            match stopOverrideBySource.TryGetValue(sourceGroup.groupId) with
            | true, values ->
                values
                |> Array.filter (fun value -> value.validFrom <= prepared.window.endDate && value.validTo >= prepared.window.startDate)
                |> Array.collect resolveStopOverride
                |> Array.distinct
            | _ -> [||]
        if overrideTarget.Length = 1 then
            if not (prepared.baseStopGroupById.ContainsKey(overrideTarget.[0])) then
                addDiagnostic prepared.diagnostics "stop_override_target_missing" sourceGroup.groupId overrideTarget.[0]
            else
                stopGroupMatches.[sourceGroup.groupId] <- overrideTarget.[0]
                stopMatchMethods.[sourceGroup.groupId] <- "reviewed_override"
        elif overrideTarget.Length > 1 then
            addDiagnostic prepared.diagnostics "stop_override_ambiguous" sourceGroup.groupId "Multiple reviewed bindings overlap the GVD"
        else
            let sourcePoints = stopGroupPoints sourceGroup
            let nearby = Dictionary<string, StopGroup>(StringComparer.Ordinal)
            for sourceLat, sourceLon in sourcePoints do
                let struct (cellLat, cellLon) = prepared.coordinateCell sourceLat sourceLon
                for latOffset in -prepared.coordinateCellRadius .. prepared.coordinateCellRadius do
                    for lonOffset in -prepared.coordinateCellRadius .. prepared.coordinateCellRadius do
                        match prepared.baseStopSpatial.TryGetValue(struct (cellLat + latOffset, cellLon + lonOffset)) with
                        | true, values ->
                            for value in values do nearby.[value.groupId] <- value
                        | _ -> ()
            let rankedCandidates =
                nearby.Values
                |> Seq.choose (fun target ->
                    match stopNameMatchRank sourceGroup.name target.name with
                    | Some rank
                        when stopGroupPoints target
                             |> Array.exists (fun (targetLat, targetLon) ->
                                 sourcePoints
                                 |> Array.exists (fun (sourceLat, sourceLon) ->
                                     haversineMetres (float sourceLat) (float sourceLon) (float targetLat) (float targetLon)
                                     <= prepared.policy.source.stopMatch.maximumDistanceMetres)) -> Some (target, rank)
                    | _ -> None)
                |> Seq.toArray
            let candidates =
                if rankedCandidates.Length = 0 then [||]
                else
                    let bestRank = rankedCandidates |> Array.minBy snd |> snd
                    rankedCandidates
                    |> Array.choose (fun (target, rank) -> if rank = bestRank then Some target else None)
            if candidates.Length = 1 then
                stopGroupMatches.[sourceGroup.groupId] <- candidates.[0].groupId
                stopMatchMethods.[sourceGroup.groupId] <- "name_geo_unique"
                prepared.stopGroupDistance sourceGroup candidates.[0] |> Option.iter (fun distance -> stopMatchDistances.[sourceGroup.groupId] <- distance)
            elif candidates.Length > 1 then
                addDiagnostic prepared.diagnostics "stop_match_ambiguous" sourceGroup.groupId $"{candidates.Length} name/geography candidates"
            else
                addDiagnostic prepared.diagnostics "stop_match_unresolved" sourceGroup.groupId "No unique name/geography candidate"
    logProgress "match-stop-groups" (int64 sourceStopGroups.Length) (Some (int64 sourceStopGroups.Length))
    Log.Information(
        "Regional overlay stop matching: matched_groups={MatchedGroups}; unresolved_or_ambiguous_groups={UnmatchedGroups}",
        stopGroupMatches.Count, sourceStopGroups.Length - stopGroupMatches.Count)

    let mutable mappedPlaceBySourceStop =
        sourceStopRows
        |> Array.choose (fun row ->
            let sourceStopId = rowValue row "stop_id"
            match sourceGroupByMember.TryGetValue(sourceStopId) with
            | true, groupId ->
                match stopGroupMatches.TryGetValue(groupId) with
                | true, target -> Some (sourceStopId, target)
                | _ -> None
            | _ -> None)
        |> Map.ofArray
    {
        stopGroupMatches = stopGroupMatches
        stopMatchMethods = stopMatchMethods
        stopMatchDistances = stopMatchDistances
        mappedPlaceBySourceStop = mappedPlaceBySourceStop
    }
