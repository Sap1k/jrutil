// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// fix-jdf: geocode one source JDF batch and classify its international routes.
module JrUtil.JdfFix

open System
open System.Collections.Generic
open Serilog

/// The calls of the trip with the most calls (the first occurrence breaks ties).
let longestTripStops (tripStops: JdfModel.TripStop array) =
    if Array.isEmpty tripStops then
        invalidOp "Cannot select a representative trip from an empty TripStop relation"
    let counts = Dictionary<_, struct (int * int)>()
    tripStops
    |> Array.iteri (fun index tripStop ->
        let key = tripStop.routeId, tripStop.tripId
        match counts.TryGetValue(key) with
        | true, struct (count, first) -> counts.[key] <- struct (count + 1, first)
        | _ -> counts.[key] <- struct (1, index))
    let mutable selected = tripStops.[0].routeId, tripStops.[0].tripId
    let mutable selectedCount = -1
    let mutable selectedFirst = Int32.MaxValue
    for KeyValue(key, struct (count, first)) in counts do
        if count > selectedCount || (count = selectedCount && first < selectedFirst) then
            selected <- key
            selectedCount <- count
            selectedFirst <- first
    tripStops
    |> Array.filter (fun tripStop ->
        (tripStop.routeId, tripStop.tripId) = selected)

/// One fixed source batch: the batch to write, the international route
/// decisions of the source batch and its stop-matching geography counters.
type FixedBatch = {
    batch: JdfModel.JdfBatch
    internationalRouteDecisions: JdfGtfsRules.InternationalRouteDecision array
    matchDiagnostics: JdfFixups.MatchDiagnostics
}

/// Fix one parsed source batch.
let fixBatch stopMatcher internationalRoutePolicy collectEstimatedPostEvidence
             (batch: JdfModel.JdfBatch) =
    let routeFilter =
        JdfInternationalFilter.applyInternationalRoutePolicy
            internationalRoutePolicy batch
    let inferredBatch, matchDiagnostics =
        match JdfFixups.dropDegenerateBatch batch with
        | Some emptyBatch ->
            Log.Warning(
                "Dropping degenerate JDF batch with fewer than two distinct called stops")
            emptyBatch, JdfFixups.MatchDiagnostics.create ()
        | None ->
            // Route classification can change after batches with
            // the same distinction are merged. Always geocode a
            // non-degenerate source batch.
            let batchFixed, stopMatches, matchDiagnostics =
                JdfFixups.fixPublicCisJrBatch stopMatcher batch
            let stopsWithMatches =
                Array.zip batchFixed.stops stopMatches
                |> JdfFixups.rejectImplausibleMatches (batchFixed.tripStops |> Seq.toArray)
            let retainedCandidateStops =
                stopsWithMatches
                |> Seq.choose (fun (stop, match_) ->
                    match_ |> Option.map (fun _ -> stop.id))
                |> Set
            let batchFixed = {
                batchFixed with
                    postCandidateEvidence =
                        batchFixed.postCandidateEvidence
                        |> Array.filter (fun observation ->
                            retainedCandidateStops.Contains(observation.stopId))
            }
            JdfModel.validatePostCandidateEvidence batchFixed.postCandidateEvidence
            Seq.concat [
                // Take one trip most likely to contain all stops'
                // km distances (testing all takes too much time).
                JdfFixups.checkMatchDistances
                    (longestTripStops (batchFixed.tripStops |> Seq.toArray)) stopsWithMatches

                JdfFixups.checkMissingRegionsCountries batchFixed
            ]
            |> Seq.iter (fun msg -> Log.Write(msg))
            JdfFixups.addStopLocations batchFixed stopsWithMatches
            |> JdfFixups.estimateMissingStopLocations,
            matchDiagnostics
    let batchWithLocations =
        if collectEstimatedPostEvidence then inferredBatch
        else { inferredBatch with
                   postCandidateEvidence = [||] }
    { batch = batchWithLocations
      internationalRouteDecisions = routeFilter.decisions
      matchDiagnostics = matchDiagnostics }
