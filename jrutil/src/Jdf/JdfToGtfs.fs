// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.JdfToGtfs

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open FSharp.Data
open NetTopologySuite.Geometries
open NodaTime
open NodaTime.Calendars
open Serilog

open JrUtil
open JrUtil.Holidays
open JrUtil.GeoData.Common
open JrUtil.GeoData.Osm

type InternationalRoutePolicy =
    | KeepAll
    | RegionalAdjacent

type InternationalRouteOverrideDecision =
    | KeepRoute
    | DropRoute

type InternationalRouteOverride = {
    routeId: string
    routeDistinction: int
    decision: InternationalRouteOverrideDecision
    reason: string
}

type InternationalRouteDecision = {
    routeId: string
    routeDistinction: int
    keep: bool
    reason: string
    countries: string array
    maximumTripSpanKm: decimal option
    maximumForeignDepthKm: decimal option
    integrated: bool
    overrideDecision: InternationalRouteOverrideDecision option
    retainedDomesticTrips: int
    qualifyingCrossBorderTrips: int
    rejectedCrossBorderTrips: int
    foreignOnlyTrips: int
}

type InternationalRouteFilterResult = {
    batch: JdfModel.JdfBatch
    decisions: InternationalRouteDecision array
}

type TransportModeRule = {
    agencyId: string
    routeIdFrom: int
    routeIdTo: int
    publicLineFrom: int
    publicLineTo: int
    expectedMode: JdfModel.TransportMode
    effectiveMode: JdfModel.TransportMode
    reason: string
}

type TransportModeRuleSet = {
    sha256: string option
    rules: TransportModeRule array
}

type TransportModeDecision = {
    routeId: string
    routeDistinction: int
    corrected: bool
    message: string
    effectiveMode: JdfModel.TransportMode
}

let emptyTransportModeRules = { sha256 = None; rules = [||] }

let private parseTransportMode argument = function
    | "A" -> JdfModel.Bus
    | "E" -> JdfModel.Tram
    | "T" -> JdfModel.Trolleybus
    | "L" -> JdfModel.CableCar
    | "M" -> JdfModel.Metro
    | "P" -> JdfModel.Ferry
    | value -> invalidArg argument $"Unknown JDF transport mode: {value}"

let loadTransportModeRules (path: string) =
    let expected = [| "agency_id"; "route_id_from"; "route_id_to"; "public_line_from";
                      "public_line_to"; "expected_mode"; "effective_mode"; "reason" |]
    let bytes = File.ReadAllBytes(path)
    let csv: CsvFile = CsvFile.Parse(System.Text.Encoding.UTF8.GetString(bytes), hasHeaders = true)
    if csv.Headers <> Some expected then
        let expectedHeader = String.Join(",", expected)
        invalidArg "transportModeRules" $"Transport mode rule CSV must have header {expectedHeader}"
    let integer (field: string) (value: string) =
        match Int32.TryParse(value.Trim()) with
        | true, parsed -> parsed
        | _ -> invalidArg "transportModeRules" $"Invalid {field}: {value}"
    let rules =
        csv.Rows
        |> Seq.map (fun row ->
            let reason = row.[7].Trim()
            if String.IsNullOrWhiteSpace(row.[0]) || String.IsNullOrWhiteSpace(reason) then
                invalidArg "transportModeRules" "Rule agency_id and reason are required"
            let rule = {
                agencyId = row.[0].Trim()
                routeIdFrom = integer "route_id_from" row.[1]
                routeIdTo = integer "route_id_to" row.[2]
                publicLineFrom = integer "public_line_from" row.[3]
                publicLineTo = integer "public_line_to" row.[4]
                expectedMode = parseTransportMode "transportModeRules" (row.[5].Trim())
                effectiveMode = parseTransportMode "transportModeRules" (row.[6].Trim())
                reason = reason }
            if rule.routeIdFrom > rule.routeIdTo || rule.publicLineFrom > rule.publicLineTo then
                invalidArg "transportModeRules" "Rule ranges must be ascending"
            rule)
        |> Seq.toArray
    { sha256 = Some (Convert.ToHexString(SHA256.HashData(bytes)).ToLowerInvariant()); rules = rules }

let internationalRoutePolicyName = function
    | KeepAll -> "keep-all"
    | RegionalAdjacent -> "regional-adjacent"

let parseInternationalRoutePolicy = function
    | null | "" | "keep-all" -> KeepAll
    | "regional-adjacent" -> RegionalAdjacent
    | value -> invalidArg "internationalRoutePolicy" $"Unknown international route policy: {value}"

let loadInternationalRouteOverrides (path: string) =
    let expected = [| "route_id"; "route_distinction"; "decision"; "reason" |]
    let csv: CsvFile = CsvFile.Parse(File.ReadAllText(path), hasHeaders = true)
    if csv.Headers <> Some expected then
        let expectedHeader = String.Join(",", expected)
        invalidArg "internationalRouteOverrides"
            $"International route override CSV must have header {expectedHeader}"
    let parsed =
        csv.Rows
        |> Seq.map (fun (row: CsvRow) ->
            let routeId = row.[0].Trim()
            let distinction =
                match Int32.TryParse(row.[1].Trim()) with
                | true, value -> value
                | _ -> invalidArg "internationalRouteOverrides"
                           $"Invalid route_distinction {row.[1]} for route {routeId}"
            let decision =
                match row.[2].Trim().ToLowerInvariant() with
                | "keep" -> KeepRoute
                | "drop" -> DropRoute
                | value -> invalidArg "internationalRouteOverrides"
                               $"Invalid decision {value} for route {routeId}/{distinction}"
            let reason = row.[3].Trim()
            if String.IsNullOrWhiteSpace(routeId) || String.IsNullOrWhiteSpace(reason) then
                invalidArg "internationalRouteOverrides" "Override route_id and reason are required"
            { routeId = routeId; routeDistinction = distinction
              decision = decision; reason = reason })
        |> Seq.toArray
    parsed
    |> Seq.groupBy (fun item -> item.routeId, item.routeDistinction)
    |> Seq.iter (fun ((routeId, distinction), values) ->
        let decisions = values |> Seq.map (fun value -> value.decision) |> Seq.distinct |> Seq.length
        if decisions > 1 then
            invalidArg "internationalRouteOverrides"
                $"Conflicting overrides for route {routeId}/{distinction}")
    parsed
    |> Seq.distinctBy (fun item -> item.routeId, item.routeDistinction, item.decision)
    |> Seq.toArray

// Information that is lost in the conversion:
// Textual notes about routes
// "Označení časového kódu" - attributes stored in ServiceNote
// Details of accessibility attributes
// Transfer attributes
// "Stop exclusivity" attributes (can't go A->C, or C->B, but B->C is fine)
// TripGroups
// And probably even more things. These are just the ones that are likely
// to become a problem.

let jdfAgencyId (id: string) idDistinction =
    sprintf "jdf:agency:%s:%d" (Uri.EscapeDataString(id)) idDistinction

let jdfStopId cis id =
    if cis
    // Stop IDs which are global (from the CIS database)
    then sprintf "cis:stop:%d" id
    // Stop IDs which are local to the file
    else sprintf "jdf:stop:%d" id

let jdfUnspecifiedStopId cis id =
    sprintf "%s:unspecified" (jdfStopId cis id)

let jdfStopPostId cis stopId stopPostId =
    sprintf "%s:post:id:%d" (jdfStopId cis stopId) stopPostId

let jdfStopPostNumId cis stopId (stopPostNum: string) =
    sprintf "%s:post:%s"
            (jdfStopId cis stopId)
            (Uri.EscapeDataString(stopPostNum))

let jdfRouteId (id: string) idDistinction =
    sprintf "jdf:route:%s:%d" (Uri.EscapeDataString(id)) idDistinction

let jdfSourceRouteId (id: string) idDistinction =
    jdfRouteId id idDistinction

let jdfSourceZoneId (routeId: string) routeDistinction (zoneCode: string) =
    sprintf "jdf:zone:%s:%d:%s"
            (Uri.EscapeDataString(routeId))
            routeDistinction
            (Uri.EscapeDataString(zoneCode))

let jdfTripId (routeId: string) routeDistinction id =
    sprintf "jdf:trip:%s:%d:%d"
            (Uri.EscapeDataString(routeId)) routeDistinction id

let getGtfsRouteType (jdfRoute: JdfModel.Route) =
    match (jdfRoute.transportMode, jdfRoute.routeType) with
    | (JdfModel.Bus, JdfModel.City)
    | (JdfModel.Bus, JdfModel.CityAndAdjacent) -> "704" // Local bus
    | (JdfModel.Bus, JdfModel.International)
    | (JdfModel.Bus, JdfModel.InternationalNoNational) // International coach
    | (JdfModel.Bus, JdfModel.InternationalOrNational) -> "201"
    | (JdfModel.Bus, JdfModel.ExtraDistrict)
    | (JdfModel.Bus, JdfModel.Regional)
    | (JdfModel.Bus, JdfModel.ExtraRegional)
    | (JdfModel.Bus, JdfModel.RegionalInternational) -> "701" // Regional bus
    | (JdfModel.Bus, JdfModel.LongDistanceNational) -> "202" // National coach
    | (JdfModel.Tram, _) -> "900" // Tram
    | (JdfModel.CableCar, _) -> "1701" // Cable car
    | (JdfModel.Metro, _) -> "401" // Metro (TODO?)
    | (JdfModel.Ferry, _) -> "1000" // Water transport
    | (JdfModel.Trolleybus, _) -> "800" // Trolleybus

let getGtfsRouteColors (publicLineNumber: string option)
                       (jdfRoute: JdfModel.Route) =
    let colors background text = Some background, Some text
    let colorsWithWhiteText background = colors background "ffffff"
    match jdfRoute.transportMode with
    | JdfModel.Bus ->
        match jdfRoute.routeType with
        | JdfModel.International
        | JdfModel.InternationalNoNational
        | JdfModel.InternationalOrNational
        | JdfModel.LongDistanceNational -> colorsWithWhiteText "004f71"
        | JdfModel.RegionalInternational -> colorsWithWhiteText "00695c"
        | _ -> colorsWithWhiteText "0076a3"
    | JdfModel.Tram -> colorsWithWhiteText "7a0200"
    | JdfModel.CableCar -> colors "c8d021" "1c1745"
    | JdfModel.Trolleybus -> colorsWithWhiteText "80166f"
    | JdfModel.Metro ->
        match publicLineNumber |> Option.map (fun value -> value.ToUpperInvariant()) with
        | Some "A" -> colorsWithWhiteText "00b274"
        | Some "B" -> colors "fbaf33" "1c1745"
        | Some "C" -> colorsWithWhiteText "d31245"
        | _ -> colorsWithWhiteText "1c1745"
    | JdfModel.Ferry -> colors "00b3cb" "1c1745"

let getStopName (jdfStop: JdfModel.Stop) =
    // This tries to mimic how IDOS displays these names
    match (jdfStop.district, jdfStop.nearbyPlace) with
    | (None, None) -> jdfStop.town
    | (Some d, None) -> sprintf "%s,%s" jdfStop.town d
    | (None, Some np) -> sprintf "%s,,%s" jdfStop.town np
    | (Some d, Some np) -> sprintf "%s,%s,%s" jdfStop.town d np

let nonEmptyTrimmed (value: string) =
    if String.IsNullOrWhiteSpace(value) then None
    else Some (value.Trim())

let normalizeNumericDesignation (value: string) =
    let withoutZeros = value.TrimStart('0')
    if withoutZeros = "" then "0" else withoutZeros

let getPublicLineNumbers (jdfBatch: JdfModel.JdfBatch) =
    let integrationsByRoute =
        jdfBatch.routeIntegrations
        |> Seq.groupBy (fun ri -> ri.routeId, ri.routeDistinction)
        |> Map

    jdfBatch.routes
    |> Seq.map (fun route ->
        let key = route.id, route.idDistinction
        let preferred =
            integrationsByRoute
            |> Map.tryFind key
            |> Option.defaultValue Seq.empty
            |> Seq.filter (fun ri -> ri.preferential)
            |> Seq.toArray

        let normalizeExplicit value =
            value
            |> nonEmptyTrimmed
            |> Option.map (fun designation ->
                if Regex.IsMatch(designation, "^[0-9]+$")
                then normalizeNumericDesignation designation
                else designation)

        let publicLineNumber =
            match preferred with
            | [| integration |] ->
                match normalizeExplicit integration.routeName with
                | Some value -> Some value
                | None ->
                    Log.Warning(
                        "JDF route {RouteId}/{RouteDistinction} has an empty preferred LinExt designation",
                        route.id, route.idDistinction)
                    None
            | [||] ->
                if Regex.IsMatch(route.id, "^[0-9]{6}$") then
                    route.id.Substring(3)
                    |> normalizeNumericDesignation
                    |> Some
                else
                    Log.Warning(
                        "JDF route {RouteId}/{RouteDistinction} has no preferred LinExt designation and its CIS line ID is not six digits",
                        route.id, route.idDistinction)
                    None
            | _ ->
                Log.Warning(
                    "JDF route {RouteId}/{RouteDistinction} has {PreferredCount} preferred LinExt designations",
                    route.id, route.idDistinction, preferred.Length)
                None
        key, publicLineNumber)
    |> Map

let applyTransportModeRules (ruleSet: TransportModeRuleSet) (batch: JdfModel.JdfBatch) =
    let publicLines = getPublicLineNumbers batch
    let decisions = ResizeArray<TransportModeDecision>()
    let routes =
        batch.routes
        |> Array.map (fun route ->
            let numericRoute = match Int32.TryParse(route.id) with true, value -> Some value | _ -> None
            let publicLine =
                publicLines.[route.id, route.idDistinction]
                |> Option.bind (fun value -> match Int32.TryParse(value) with true, parsed -> Some parsed | _ -> None)
            let candidates =
                ruleSet.rules
                |> Array.filter (fun rule ->
                    rule.agencyId = route.agencyId
                    && numericRoute |> Option.exists (fun value -> value >= rule.routeIdFrom && value <= rule.routeIdTo))
            let matched =
                candidates
                |> Array.tryFind (fun rule ->
                    route.transportMode = rule.expectedMode
                    && publicLine |> Option.exists (fun value -> value >= rule.publicLineFrom && value <= rule.publicLineTo))
            match matched with
            | Some rule ->
                decisions.Add {
                    routeId = route.id; routeDistinction = route.idDistinction; corrected = true
                    message = rule.reason; effectiveMode = rule.effectiveMode }
                { route with transportMode = rule.effectiveMode }
            | None when candidates.Length > 0 ->
                decisions.Add {
                    routeId = route.id; routeDistinction = route.idDistinction; corrected = false
                    message = "reviewed rule guard mismatch"; effectiveMode = route.transportMode }
                route
            | None -> route)
    { batch with routes = routes }, decisions.ToArray()

let derivedStopPosts (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.tripStops
    |> Seq.choose (fun tripStop ->
        match tripStop.stopPostId, tripStop.stopPostNum |> Option.bind nonEmptyTrimmed with
        | None, Some stopPostNum -> Some (tripStop.stopId, stopPostNum)
        | _ -> None)
    |> Seq.distinct
    |> Seq.toArray

let stopPostNumbersById (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.tripStops
    |> Seq.choose (fun tripStop ->
        match tripStop.stopPostId, tripStop.stopPostNum |> Option.bind nonEmptyTrimmed with
        | Some stopPostId, Some stopPostNum ->
            Some ((tripStop.stopId, stopPostId), stopPostNum)
        | _ -> None)
    |> Seq.groupBy fst
    |> Seq.choose (fun (key, values) ->
        let numbers = values |> Seq.map snd |> Seq.distinct |> Seq.toArray
        match numbers with
        | [| number |] -> Some (key, number)
        | [||] -> None
        | _ ->
            let stopId, stopPostId = key
            Log.Warning(
                "JDF stop post {StopId}/{StopPostId} has conflicting station numbers {StationNumbers}",
                stopId, stopPostId, String.Join(",", numbers))
            None)
    |> Map

type PostResolution =
    | Physical of candidateId: string
    | Side of sideGroupId: string * representativeCandidateId: string
    | Centroid

type DerivedPostSelection = {
    locationId: string
    stopId: int64
    candidateIds: string array
    sideGroupId: string option
    representativeCandidateId: string option
    resolution: PostResolution
    lat: decimal
    lon: decimal
    selectionKind: string
    score: float
    margin: float option
}

type PostSideGroup = {
    sideGroupId: string
    stopId: int64
    mode: JdfModel.TransportMode
    corridorFaceId: string
    sector: string
    representativeCandidateId: string
    memberCandidateIds: string array
    compactnessMetres: float
    repeatedPatternSupport: int
}

type DerivedPostContext = {
    previousStopId: int64 option
    nextStopId: int64 option
    sameStopBlockRole: string
}

[<Struct>]
type PostPatternContextKey = {
    stopId: int64
    mode: JdfModel.TransportMode
    lineId: string
    routeDistinction: int
    direction: int
    patternHash: string
    position: int
    sameStopBlockRole: string
}

type CandidateModalityEstimate = {
    candidateId: string
    supportedModes: string array
    explicitlyDeniedModes: string array
    status: string
    roadSupport: int
    trolleybusSupport: int
    tramSupport: int
    distinctSupportingPatterns: int
    evidenceMethod: string
    confidence: float
}

type PhysicalPostHypothesis = {
    hypothesisId: string
    stopId: int64
    memberObservationIds: string array
    memberCandidateIds: string array
    representativeCandidateId: string
    lat: decimal
    lon: decimal
    sources: string array
}

type DerivedPostScore = {
    context: PostPatternContextKey
    movementFamilyId: string
    candidateId: string
    eligible: bool
    alignment: float
    side: float
    proximity: float
    routedFit: float
    routedExcessMetres: float option
    corridorId: string option
    ingressThreadId: string option
    egressThreadId: string option
    corridorFaceId: string option
    routingAvailability: string
    alternativeCorridorCount: int
    alternativeCostGap: float option
    snapEdgeId: int option
    snapFraction: float option
    corridorDistance: float option
    signedLateralOffset: float option
    corridorHeading: float option
    attachmentHeading: float option
    tiedCorridorsAgree: bool
    topologyFailureReason: string option
    routeDistinction: int
    sourceAdjustment: float
    modalityAdjustment: float
    popularityPrior: float
    total: float
    rejectionReason: string option
}

type PostEstimationPlan = {
    authored: Map<int64 * string, DerivedPostSelection>
    calls: IReadOnlyDictionary<PostPatternContextKey, DerivedPostSelection>
    authoredContexts: Map<int64 * string, DerivedPostContext array>
    callContexts: IReadOnlyDictionary<PostPatternContextKey, DerivedPostContext>
    movementFamilyIds: IReadOnlyDictionary<PostPatternContextKey,string>
    locations: DerivedPostSelection array
    inferredLocations: DerivedPostSelection array
    inferredLocationOrdinals: Map<int64 * string, int>
    physicalHypotheses: PhysicalPostHypothesis array
    modalityEstimates: CandidateModalityEstimate array
    sideGroups: PostSideGroup array
    scoreCount: int64
    scoreRows: unit -> seq<DerivedPostScore>
    cleanupScoreRows: unit -> unit
    unresolvedPatternContexts: PostPatternContextKey array
    tripPatternHashes: IReadOnlyDictionary<struct (string * int * int64), string>
    candidateStopCount: int
    singleCandidateSkips: int
    unresolvedContexts: int
    sameStopBlocks: int
    distinctPairChoices: int
    unresolvedBlockEdges: int
}

let private emptyPostEstimationPlan = {
    authored = Map.empty
    calls = Dictionary<PostPatternContextKey, DerivedPostSelection>()
    authoredContexts = Map.empty
    callContexts = Dictionary<PostPatternContextKey, DerivedPostContext>()
    movementFamilyIds = Dictionary<PostPatternContextKey,string>()
    locations = [||]
    inferredLocations = [||]
    inferredLocationOrdinals = Map.empty
    physicalHypotheses = [||]
    modalityEstimates = [||]
    sideGroups = [||]
    scoreCount = 0L
    scoreRows = fun () -> Seq.empty
    cleanupScoreRows = ignore
    unresolvedPatternContexts = [||]
    tripPatternHashes = Dictionary<struct (string * int * int64), string>()
    candidateStopCount = 0; singleCandidateSkips = 0
    unresolvedContexts = 0; sameStopBlocks = 0; distinctPairChoices = 0
    unresolvedBlockEdges = 0
}

/// Adapts the inference-domain result to the converter's publishing plan.
/// Policy evaluation remains entirely in JdfPostInferenceEvaluator; this
/// function only translates already-decided assignments and diagnostics.
let postEstimationPlanFromInferenceResult
        (result:JdfPostInference.PostInferenceResult) : PostEstimationPlan =
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "result-adaptation"
    let disposeResult () = (result :> IDisposable).Dispose()
    try
        let hypothesesById =
            result.Hypotheses
            |> Array.map(fun value -> value.hypothesisId,value)
            |> Map.ofArray
        let sideGroupsById =
            result.SideGroups
            |> Array.map(fun value -> value.sideGroupId,value)
            |> Map.ofArray
        let physicalHypotheses =
            result.Hypotheses
            |> Array.map(fun value ->
                ({ hypothesisId=value.hypothesisId
                   stopId=value.stopId
                   memberObservationIds=value.memberObservationIds
                   memberCandidateIds=value.memberRoutePointIds
                   representativeCandidateId=value.representativeRoutePointId
                   lat=decimal value.latitude
                   lon=decimal value.longitude
                   sources=[||] } : PhysicalPostHypothesis))
        let sideGroups =
            result.SideGroups
            |> Array.map(fun value ->
                ({ sideGroupId=value.sideGroupId
                   stopId=value.stopId
                   mode=parseTransportMode "inference result mode" value.mode
                   corridorFaceId=value.corridorFaceId
                   sector=value.sector
                   representativeCandidateId=value.representativeHypothesisId
                   memberCandidateIds=value.memberHypothesisIds
                   compactnessMetres=value.compactnessMetres
                   repeatedPatternSupport=value.support } : PostSideGroup))
        let selectionFor (assignment:JdfPostInference.ContextPostAssignment) =
            match assignment.selectedLocationId,assignment.selectedHypothesisId with
            | Some locationId,Some hypothesisId ->
                match assignment.selectedSideGroupId with
                | Some groupId ->
                    let group=sideGroupsById.[groupId]
                    Some ({ locationId=locationId;stopId=assignment.stopId
                            candidateIds=[|group.representativeHypothesisId|]
                            sideGroupId=Some groupId
                            representativeCandidateId=Some group.representativeHypothesisId
                            resolution=Side(groupId,group.representativeHypothesisId)
                            lat=decimal group.latitude;lon=decimal group.longitude
                            selectionKind="side"
                            score=assignment.score |> Option.defaultValue 0.0
                            margin=assignment.margin } : DerivedPostSelection)
                | None ->
                    let hypothesis=hypothesesById.[hypothesisId]
                    Some ({ locationId=locationId;stopId=assignment.stopId
                            candidateIds=[|hypothesisId|];sideGroupId=None
                            representativeCandidateId=Some hypothesisId
                            resolution=Physical hypothesisId
                            lat=decimal hypothesis.latitude;lon=decimal hypothesis.longitude
                            selectionKind="physical"
                            score=assignment.score |> Option.defaultValue 0.0
                            margin=assignment.margin } : DerivedPostSelection)
            | _ -> None
        let calls=Dictionary<PostPatternContextKey,DerivedPostSelection>()
        let callContexts=Dictionary<PostPatternContextKey,DerivedPostContext>()
        let movementFamilyIds=Dictionary<PostPatternContextKey,string>()
        let unresolved=ResizeArray<PostPatternContextKey>()
        let selectionsByLocation=Dictionary<string,DerivedPostSelection>(StringComparer.Ordinal)
        let authoredContexts=ResizeArray<(int64*string)*DerivedPostContext>()
        for assignment in result.Assignments.ReadRows() do
            let contextKey:PostPatternContextKey = {
                stopId=assignment.stopId
                mode=parseTransportMode "inference result mode" assignment.mode
                lineId=assignment.lineId;routeDistinction=assignment.routeDistinction
                direction=assignment.direction;patternHash=assignment.patternHash
                position=assignment.patternPosition
                sameStopBlockRole=assignment.sameStopBlockRole }
            let context:DerivedPostContext = {
                previousStopId=assignment.previousStopId
                nextStopId=assignment.nextStopId
                sameStopBlockRole=assignment.sameStopBlockRole }
            match assignment.assignmentKind,assignment.authoredPostKey with
            | "authored",Some authoredKey ->
                authoredContexts.Add((assignment.stopId,authoredKey),context)
            | "unlabelled",_ ->
                callContexts.[contextKey] <- context
                movementFamilyIds.[contextKey] <- assignment.movementFamilyId
                match selectionFor assignment with
                | Some selection ->
                    calls.[contextKey] <- selection
                    selectionsByLocation.TryAdd(selection.locationId,selection) |> ignore
                | None -> unresolved.Add(contextKey)
            | _ -> ()
        let inferredLocations =
            selectionsByLocation.Values |> Seq.sortBy _.locationId |> Seq.toArray
        let inferredLocationOrdinals =
            inferredLocations
            |> Seq.groupBy _.stopId
            |> Seq.collect (fun (stopId, selections) ->
                selections
                |> Seq.map _.locationId
                |> Seq.distinct
                |> Seq.sortWith (fun left right -> StringComparer.Ordinal.Compare(left, right))
                |> Seq.mapi (fun index locationId ->
                    (stopId, locationId), index + 1))
            |> Map.ofSeq
        let authored =
            result.AuthoredPositions
            |> Array.choose(fun value ->
                match selectionsByLocation.TryGetValue(value.locationId) with
                | true,selection -> Some((value.stopId,value.authoredPostKey),selection)
                | _ -> None)
            |> Map.ofArray
        let authoredContextMap =
            authoredContexts
            |> Seq.groupBy fst
            |> Seq.map(fun (key,values) ->
                key,
                (values
                 |> Seq.map snd
                 |> Seq.distinct
                 |> Seq.sortBy(fun value ->
                     value.sameStopBlockRole,value.previousStopId,value.nextStopId)
                 |> Seq.toArray))
            |> Map.ofSeq
        let scoreRows () = seq {
            use assignments=result.Assignments.ReadRows().GetEnumerator()
            let mutable hasAssignment=assignments.MoveNext()
            for score in result.DiagnosticScores.ReadRows() do
                while hasAssignment && assignments.Current.contextId<>score.contextId do
                    hasAssignment<-assignments.MoveNext()
                if not hasAssignment then
                    invalidOp $"Derived post score references an unknown context: {score.contextId}"
                let assignment=assignments.Current
                let context:PostPatternContextKey = {
                    stopId=assignment.stopId
                    mode=parseTransportMode "inference result mode" assignment.mode
                    lineId=assignment.lineId;routeDistinction=assignment.routeDistinction
                    direction=assignment.direction;patternHash=assignment.patternHash
                    position=assignment.patternPosition
                    sameStopBlockRole=assignment.sameStopBlockRole }
                yield ({ context=context;movementFamilyId=assignment.movementFamilyId
                         candidateId=score.candidateId;eligible=score.eligible
                         alignment=score.alignment;side=score.side;proximity=score.proximity
                         routedFit=score.routedExcess;routedExcessMetres=score.routedExcessMetres
                         corridorId=score.corridorId;ingressThreadId=score.ingressThreadId
                         egressThreadId=score.egressThreadId;corridorFaceId=score.corridorFaceId
                         routingAvailability=score.routingAvailability
                         alternativeCorridorCount=score.alternativeCorridorCount
                         alternativeCostGap=score.alternativeCostGap
                         snapEdgeId=score.snapEdgeId;snapFraction=score.snapFraction
                         corridorDistance=score.corridorDistance
                         signedLateralOffset=score.signedLateralOffset
                         corridorHeading=score.corridorHeading
                         attachmentHeading=score.attachmentHeading
                         tiedCorridorsAgree=score.tiedCorridorsAgree
                         topologyFailureReason=score.topologyFailureReason
                         routeDistinction=assignment.routeDistinction
                         sourceAdjustment=score.sourceAdjustment
                         modalityAdjustment=score.modalityAdjustment
                         popularityPrior=score.popularityAdjustment
                         total=score.total;rejectionReason=score.rejectionReason }
                       : DerivedPostScore)
        }
        { authored=authored;calls=calls;authoredContexts=authoredContextMap
          callContexts=callContexts;movementFamilyIds=movementFamilyIds
          locations=inferredLocations;inferredLocations=inferredLocations
          inferredLocationOrdinals=inferredLocationOrdinals
          physicalHypotheses=physicalHypotheses;modalityEstimates=[||]
          sideGroups=sideGroups;scoreCount=result.DiagnosticScores.Count
          scoreRows=scoreRows;cleanupScoreRows=disposeResult
          unresolvedPatternContexts=unresolved.ToArray()
          tripPatternHashes=Dictionary<struct(string*int*int64),string>()
          candidateStopCount=result.Counters.candidateStopCount;singleCandidateSkips=0
          unresolvedContexts=result.Counters.unresolvedContexts
          sameStopBlocks=result.Counters.sameStopBlocks
          distinctPairChoices=result.Counters.distinctPairChoices
          unresolvedBlockEdges=result.Counters.unresolvedBlockEdges }
    with _ ->
        disposeResult()
        reraise()

let private authoredPostKey (call: JdfModel.TripStop) =
    match call.stopPostId, call.stopPostNum |> Option.bind nonEmptyTrimmed with
    | Some value, _ -> Some $"id:{value}"
    | None, Some value -> Some $"num:{value}"
    | _ -> None

let private completePatternHash (calls: JdfModel.TripStop array) =
    calls
    |> Array.map (fun call -> string call.stopId)
    |> String.concat ","
    |> Encoding.UTF8.GetBytes
    |> SHA256.HashData
    |> Convert.ToHexString
    |> fun value -> value.ToLowerInvariant()

let private callIsUsable (call: JdfModel.TripStop) =
    match call.departureTime, call.arrivalTime with
    | Some JdfModel.Passing, _ | Some JdfModel.NotPassing, _ -> false
    | None, None -> false
    | _ -> true

let buildPostEstimationPlan (batch: JdfModel.JdfBatch) =
    ignore batch
    emptyPostEstimationPlan

// TODO: Naming in this whole module
let convertToGtfsAgency: JdfModel.Agency -> GtfsModel.Agency = fun jdfAgency ->
    {
        // Unfortunately in JDF most primary keys are split
        // between two fields, so we just concatenate them
        id = Some (jdfAgencyId jdfAgency.id jdfAgency.idDistinction)
        name = jdfAgency.name
        url =
            jdfAgency.website
            |> Option.map (fun url ->
                if not <| Regex.IsMatch(url, @"https?://")
                then "http://" + url
                else url
            )
        // This will have to be adjusted for slovak datasets
        timezone = "Europe/Prague"
        lang = Some "cs"
        phone = Some jdfAgency.officePhoneNum
        fareUrl = None
        email = jdfAgency.email
    }

let inferredPostId stopIdsCis (plan: PostEstimationPlan) (selection: DerivedPostSelection) =
    match plan.inferredLocationOrdinals |> Map.tryFind (selection.stopId, selection.locationId) with
    | Some ordinal -> $"{jdfStopId stopIdsCis selection.stopId}:est:{ordinal}"
    | None ->
        invalidOp
            $"Inferred JDF post has no deterministic ordinal: stop={selection.stopId}; location={selection.locationId}"

let private getGtfsStopsWithPlan stopIdsCis (plan: PostEstimationPlan)
                                    (jdfBatch: JdfModel.JdfBatch) =
    let stopLocationsById =
        jdfBatch.stopLocations
        |> Seq.groupBy (fun sl -> sl.stopId)
        |> Seq.map (fun (stopId, locations) ->
            let location =
                locations
                |> Seq.sortBy (fun sl ->
                    match sl.precision with
                    | JdfModel.StopPrecise -> 0
                    | JdfModel.Estimated -> 1)
                |> Seq.head
            stopId, location)
        |> Map

    let zonesByStop =
        jdfBatch.routeStops
        |> Seq.collect (fun routeStop ->
            Jdf.normalizeZoneTokens [routeStop.zone]
            |> Seq.map (fun zoneCode ->
                routeStop.stopId,
                jdfSourceZoneId routeStop.routeId
                                routeStop.routeDistinction
                                zoneCode,
                zoneCode))
        |> Seq.groupBy (fun (stopId, _, _) -> stopId)
        |> Seq.map (fun (stopId, memberships) ->
            let memberships =
                memberships
                |> Seq.map (fun (_, zoneId, zoneCode) -> zoneId, zoneCode)
                |> Seq.distinct
                |> Seq.toArray
            stopId,
            match memberships with
            | [| _, zoneCode |] -> Some zoneCode
            | _ -> None)
        |> Map

    let gtfsStops =
        jdfBatch.stops |> Array.map (fun jdfStop ->
            let location =
                stopLocationsById
                |> Map.tryFind jdfStop.id
            let stopName =
                let name = getStopName jdfStop
                match location with
                | Some value when value.precision <> JdfModel.StopPrecise ->
                    Gtfs.markApproximateStopName name
                | _ -> name
            {
                id = jdfStopId stopIdsCis jdfStop.id
                code = None
                name = stopName
                description = None
                lat = location |> Option.map (fun value -> value.lat)
                lon = location |> Option.map (fun value -> value.lon)
                // GTFS only permits one zone_id. Plural memberships are
                // represented losslessly in cz_stop_zones.txt.
                zoneId = zonesByStop |> Map.tryFind jdfStop.id |> Option.flatten
                url = None
                locationType = Some GtfsModel.Station
                parentStation = None
                // TODO: Try to guess from jdfStop.country
                timezone = Some "Europe/Prague"

                wheelchairBoarding =
                    // This doesn't use "2" ("not possible"), because there's no
                    // corresponding JDF attribute
                    if jdfStop.attributes
                       |> Jdf.parseAttributes jdfBatch
                       |> Set.contains JdfModel.WheelchairAccessible
                    then Some 1
                    else Some 0

                platformCode = None
            }: GtfsModel.Stop)
    let gtfsStopsById = Map <| seq { for s in gtfsStops -> s.id, s }
    let gtfsUnspecifiedStops =
        jdfBatch.stops |> Array.map (fun jdfStop ->
            let parentStop = gtfsStopsById.[jdfStopId stopIdsCis jdfStop.id]
            { parentStop with
                id = jdfUnspecifiedStopId stopIdsCis jdfStop.id
                locationType = Some GtfsModel.Stop
                parentStation = Some parentStop.id
            }: GtfsModel.Stop)
    let postNumbersById = stopPostNumbersById jdfBatch
    let gtfsStopPosts =
        jdfBatch.stopPosts |> Array.map (fun jdfStopPost ->
            let parentStop = gtfsStopsById.[jdfStopId stopIdsCis jdfStopPost.stopId]
            let selection = plan.authored |> Map.tryFind (jdfStopPost.stopId, $"id:{jdfStopPost.stopPostId}")
            { parentStop with

                id = jdfStopPostId stopIdsCis
                                   jdfStopPost.stopId
                                   jdfStopPost.stopPostId
                locationType = Some GtfsModel.Stop
                parentStation = Some parentStop.id
                platformCode =
                    jdfStopPost.postName
                    |> Option.bind nonEmptyTrimmed
                    |> Option.orElseWith (fun () ->
                        postNumbersById
                        |> Map.tryFind (jdfStopPost.stopId,
                                        jdfStopPost.stopPostId))
                    |> Option.orElse (Some (string jdfStopPost.stopPostId))
                lat = selection |> Option.map (fun value -> value.lat) |> Option.orElse parentStop.lat
                lon = selection |> Option.map (fun value -> value.lon) |> Option.orElse parentStop.lon
            }: GtfsModel.Stop)
    let gtfsNumberedStopPosts =
        derivedStopPosts jdfBatch
        |> Array.map (fun (stopId, stopPostNum) ->
            let parentStop = gtfsStopsById.[jdfStopId stopIdsCis stopId]
            let selection = plan.authored |> Map.tryFind (stopId, $"num:{stopPostNum}")
            { parentStop with
                id = jdfStopPostNumId stopIdsCis stopId stopPostNum
                locationType = Some GtfsModel.Stop
                parentStation = Some parentStop.id
                platformCode = Some stopPostNum
                lat = selection |> Option.map (fun value -> value.lat) |> Option.orElse parentStop.lat
                lon = selection |> Option.map (fun value -> value.lon) |> Option.orElse parentStop.lon
            }: GtfsModel.Stop)
    let inferredPosts =
        plan.inferredLocations
        |> Seq.sortBy (fun value -> value.stopId, value.locationId)
        |> Seq.map (fun selection ->
            let parentStop = gtfsStopsById.[jdfStopId stopIdsCis selection.stopId]
            { parentStop with
                id = inferredPostId stopIdsCis plan selection
                locationType = Some GtfsModel.Stop
                parentStation = Some parentStop.id
                platformCode = None
                lat = Some selection.lat
                lon = Some selection.lon
            }: GtfsModel.Stop)
        |> Seq.toArray

    Array.concat [ gtfsStops; gtfsUnspecifiedStops
                   gtfsStopPosts; gtfsNumberedStopPosts; inferredPosts ]

let getGtfsStops stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
    getGtfsStopsWithPlan stopIdsCis (buildPostEstimationPlan jdfBatch) jdfBatch

let getGtfsRoutesWithPublicLines
        (publicLineNumbers: Map<string * int, string option>)
                                (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.routes
    |> Array.map (fun jdfRoute ->
    let publicLineNumber =
        publicLineNumbers.[jdfRoute.id, jdfRoute.idDistinction]
    let routeColor, routeTextColor =
        getGtfsRouteColors publicLineNumber jdfRoute
    {
        id = jdfRouteId jdfRoute.id jdfRoute.idDistinction
        agencyId = Some (jdfAgencyId jdfRoute.agencyId
                                     jdfRoute.agencyDistinction)
        shortName = publicLineNumber
        longName = Some jdfRoute.name
        description = None
        // TODO: Deal with routes that don't allow national service
        // (only international)
        routeType = getGtfsRouteType jdfRoute
        url = None
        color = routeColor
        textColor = routeTextColor
        sortOrder = None
    }: GtfsModel.Route)

let getGtfsRoutes (jdfBatch: JdfModel.JdfBatch) =
    getGtfsRoutesWithPublicLines (getPublicLineNumbers jdfBatch) jdfBatch


/// Returns a boolean array, where each item represents a day in the route's
/// validity interval, true if the trip should run, false otherwise.
let tripDateBitmap (route: JdfModel.Route)
                   (trip: JdfModel.Trip)
                   (tripServiceNotes: JdfModel.ServiceNote seq)
                   (tripAttributes: JdfModel.Attribute Set) =
    let jdfNotes =
        tripServiceNotes
        // Pre-process for ease of use
        |> Seq.choose (fun sn ->
            sn.noteType |> Option.map (fun nt ->
                let df = sn.dateFrom
                         |> Option.defaultValue route.timetableValidFrom
                {|
                    noteType = nt
                    dateFrom = df
                    dateTo = sn.dateTo |> Option.defaultValue df
                |}))
        |> Seq.toArray
    let holidays =
        czechHolidayDates route.timetableValidFrom route.timetableValidTo
        |> set
    let hasDateAttribute =
        tripAttributes |> Seq.exists (fun a ->
            match a with
            | JdfModel.WeekdayService
            | JdfModel.HolidaySundayService
            | JdfModel.DayOfWeekService _ -> true
            | _ -> false)
    let hasServiceOnlyNote =
        jdfNotes |> Seq.exists (fun n -> n.noteType = JdfModel.ServiceOnly)
    let hasServiceNote =
        jdfNotes |> Seq.exists (fun n -> n.noteType = JdfModel.Service)
    Utils.dateRange route.timetableValidFrom route.timetableValidTo
    |> Seq.map (fun d ->
        let applicableNoteTypes =
            jdfNotes
            |> Seq.filter (fun sn ->
                DateInterval(sn.dateFrom, sn.dateTo).Contains(d))
            |> Seq.map (fun sn -> sn.noteType)
            |> set
        let hasNote noteType = applicableNoteTypes |> Set.contains noteType
        if hasNote JdfModel.ServiceOnly then true
        else if hasServiceOnlyNote then false
        else if hasNote JdfModel.NoService then false
        else if hasNote JdfModel.ServiceAlso then true
        else if (hasNote JdfModel.ServiceOddWeeks
                 || hasNote JdfModel.ServiceOddWeeksFromTo)
             && WeekYearRules.Iso.GetWeekOfWeekYear(d) % 2 = 0 then false
        else if (hasNote JdfModel.ServiceEvenWeeks
                 || hasNote JdfModel.ServiceEvenWeeksFromTo)
             && WeekYearRules.Iso.GetWeekOfWeekYear(d) % 2 = 1 then false
        else if (not <| hasNote JdfModel.Service)
             && hasServiceNote then false
        else if tripAttributes |> Set.contains JdfModel.HolidaySundayService
             && (holidays |> Set.contains d
                 || d.DayOfWeek = IsoDayOfWeek.Sunday) then true
        else if tripAttributes |> Set.contains JdfModel.WeekdayService
             && holidays |> Set.contains d |> not
             && d.DayOfWeek <> IsoDayOfWeek.Saturday
             && d.DayOfWeek <> IsoDayOfWeek.Sunday then true
        else if tripAttributes
                |> Set.contains (JdfModel.DayOfWeekService (
                    LanguagePrimitives.EnumToValue d.DayOfWeek)) then true
        else not hasDateAttribute)
    |> Seq.toArray

let gtfsCalendarBitmap (calendar: GtfsModel.CalendarEntry) =
    Utils.dateRange calendar.startDate calendar.endDate
    |> Seq.map (fun d ->
        let dow = LanguagePrimitives.EnumToValue d.DayOfWeek - 1
        calendar.weekdayService.[dow])
    |> Seq.toArray

type CalendarPreparation = {
    tripsToDelete: Set<string>
    calendar: GtfsModel.CalendarEntry array
    calendarExceptions: GtfsModel.CalendarException array
}

// Computes the expensive per-trip service bitmap exactly once. Bundle
// conversion carries this result through international filtering instead of
// rebuilding every bitmap for the retained batch.
let prepareGtfsCalendarWithWorkersAndProgress
        maximumWorkers
        (progress: int64 -> int64 option -> unit)
        (jdfBatch: JdfModel.JdfBatch) =
    if maximumWorkers <= 0 then invalidArg "maximumWorkers" "Calendar worker count must be positive"
    let notesByTrip =
        jdfBatch.serviceNotes
        |> Array.groupBy (fun sn -> sn.routeId, sn.routeDistinction, sn.tripId)
        |> Map

    let routeById =
        jdfBatch.routes
        |> Array.map (fun r -> (r.id, r.idDistinction), r)
        |> Map

    let calculate (jdfTrip: JdfModel.Trip) =
        let jdfRoute = routeById.[(jdfTrip.routeId, jdfTrip.routeDistinction)]
        let attrs = Jdf.parseAttributes jdfBatch jdfTrip.attributes

        let servicedDays =
            attrs
            |> Set.toList
            |> List.collect (fun a ->
                match a with
                | JdfModel.WeekdayService -> [1; 2; 3; 4; 5]
                | JdfModel.HolidaySundayService -> [7]
                | JdfModel.DayOfWeekService(d) -> [d]
                | _ -> [])
        let weekdays =
            if servicedDays.Length = 0
            then [| for i in [1..7] -> true |]
            else [| for i in [1..7] -> servicedDays
                                       |> List.contains i |]
        let tripId = jdfTripId jdfTrip.routeId
                               jdfTrip.routeDistinction
                               jdfTrip.id

        let calendarEntry: GtfsModel.CalendarEntry = {
            id = tripId
            weekdayService = weekdays
            startDate = jdfRoute.timetableValidFrom
            endDate = jdfRoute.timetableValidTo
        }

        let bitmap =
            tripDateBitmap
                jdfRoute jdfTrip
                (notesByTrip
                 |> Map.tryFind (jdfTrip.routeId,
                                 jdfTrip.routeDistinction,
                                 jdfTrip.id)
                 |> Option.defaultValue [||])
                attrs
        let calendarBitmap = gtfsCalendarBitmap calendarEntry
        let bitmapDiffCount =
            Seq.zip bitmap calendarBitmap
            |> Seq.sumBy (fun (s1, s2) -> if s1 = s2 then 0 else 1)
        let bitmapTrueCount =
            bitmap |> Array.sumBy (fun s -> if s then 1 else 0)

        // Pick the most efficient representation (calendar + exceptions vs
        // just exceptions)
        if bitmapTrueCount = 0 then
            [|tripId|], [||], [||]
        elif bitmapDiffCount > bitmapTrueCount then
            [||], [||],
            bitmap
            |> Array.indexed
            |> Array.choose (fun (i, s) ->
                if s then Some ({
                    id = tripId
                    date = jdfRoute.timetableValidFrom.PlusDays(i)
                    exceptionType = GtfsModel.ServiceAdded
                }: GtfsModel.CalendarException)
                else None)
        else
            [||], [|calendarEntry|],
            Seq.zip bitmap calendarBitmap
            |> Seq.indexed
            |> Seq.choose (fun (i, (s, sc)) ->
                if s <> sc then Some ({
                    id = tripId
                    date = jdfRoute.timetableValidFrom.PlusDays(i)
                    exceptionType = if s then GtfsModel.ServiceAdded
                                    else GtfsModel.ServiceRemoved
                }: GtfsModel.CalendarException)
                else None)
            |> Seq.toArray
    let values = Array.zeroCreate jdfBatch.trips.Length
    let mutable calculatedTrips = 0L
    let calendarProgressLock = obj()
    let mutable lastCalendarProgress = 0L
    let calculateAt index =
        values.[index] <- calculate jdfBatch.trips.[index]
        let count = Interlocked.Increment(&calculatedTrips)
        if count % 5_000L = 0L || count = int64 jdfBatch.trips.Length then
            lock calendarProgressLock (fun () ->
                if count > lastCalendarProgress then
                    lastCalendarProgress <- count
                    progress count (Some (int64 jdfBatch.trips.Length)))
    if maximumWorkers = 1 || jdfBatch.trips.Length <= 1 then
        for index = 0 to jdfBatch.trips.Length-1 do calculateAt index
    else
            let options = ParallelOptions(MaxDegreeOfParallelism = maximumWorkers)
            Parallel.For(0, jdfBatch.trips.Length, options, calculateAt)
            |> ignore
    let tripsToDelete = ResizeArray<string>()
    let calendar = ResizeArray<GtfsModel.CalendarEntry>()
    let exceptions = ResizeArray<GtfsModel.CalendarException>()
    for deleted, entries, tripExceptions in values do
        tripsToDelete.AddRange(deleted)
        calendar.AddRange(entries)
        exceptions.AddRange(tripExceptions)
    {
        tripsToDelete = set tripsToDelete
        calendar = calendar.ToArray()
        calendarExceptions = exceptions.ToArray()
    }

let prepareGtfsCalendarWithWorkers maximumWorkers (jdfBatch: JdfModel.JdfBatch) =
    prepareGtfsCalendarWithWorkersAndProgress maximumWorkers (fun _ _ -> ()) jdfBatch

let prepareGtfsCalendar (jdfBatch: JdfModel.JdfBatch) =
    prepareGtfsCalendarWithWorkers 1 jdfBatch

// Retained for source compatibility with standalone callers.
let getGtfsCalendar (jdfBatch: JdfModel.JdfBatch) =
    let prepared = prepareGtfsCalendar jdfBatch
    prepared.tripsToDelete, prepared.calendar, prepared.calendarExceptions

let filterCalendarPreparation (batch: JdfModel.JdfBatch)
                              (prepared: CalendarPreparation) =
    let retained =
        batch.trips
        |> Seq.map (fun trip -> jdfTripId trip.routeId trip.routeDistinction trip.id)
        |> HashSet
    {
        tripsToDelete = prepared.tripsToDelete |> Set.filter retained.Contains
        calendar = prepared.calendar |> Array.filter (fun value -> retained.Contains(value.id))
        calendarExceptions =
            prepared.calendarExceptions |> Array.filter (fun value -> retained.Contains(value.id))
    }

let private callIsEmitted (call: JdfModel.TripStop) =
    match call.departureTime with
    | Some JdfModel.Passing | Some JdfModel.NotPassing -> false
    | None when call.arrivalTime = None -> false
    | _ -> true

let private canonicalCountry (value: string) =
    match value.Trim().ToUpperInvariant() with
    | "AT" | "A" -> "A"
    | "DE" | "D" -> "D"
    | normalized -> normalized

let private routeIsDeclaredInternational (route: JdfModel.Route) =
    match route.routeType with
    | JdfModel.International
    | JdfModel.InternationalNoNational
    | JdfModel.InternationalOrNational -> true
    | _ -> false

let private validateInternationalRouteOverrideConflicts
        (overrides: InternationalRouteOverride array) =
    overrides
    |> Seq.groupBy (fun item -> item.routeId, item.routeDistinction)
    |> Seq.iter (fun ((routeId, distinction), values) ->
        if values |> Seq.map (fun value -> value.decision) |> Seq.distinct |> Seq.length > 1 then
            invalidArg "internationalRouteOverrides"
                $"Conflicting overrides for route {routeId}/{distinction}")

let validateInternationalRouteOverrides sourceRouteKeys
                                                (overrides: InternationalRouteOverride array) =
    validateInternationalRouteOverrideConflicts overrides
    overrides
    |> Seq.iter (fun item ->
        if not (sourceRouteKeys |> Set.contains (item.routeId, item.routeDistinction)) then
            invalidArg "internationalRouteOverrides"
                $"Override refers to unknown route {item.routeId}/{item.routeDistinction}")

type private InternationalTripDisposition =
    | DomesticTrip
    | QualifyingCrossBorderTrip of span: decimal * depth: decimal
    | RejectedCrossBorderTrip of reason: string * span: decimal option * depth: decimal option
    | ForeignOnlyTrip

let private applyInternationalRoutePolicyInternal
        maximumWorkers
        (progress: string -> int64 -> int64 option -> unit)
        (tripsToDelete: Set<string>)
        (policy: InternationalRoutePolicy)
        (overrides: InternationalRouteOverride array)
        (batch: JdfModel.JdfBatch) =
    if policy = KeepAll then { batch = batch; decisions = [||] } else

    validateInternationalRouteOverrideConflicts overrides

    let activeTripKeys = HashSet<struct (string * int * int64)>()
    for trip in batch.trips do
        let gtfsId = jdfTripId trip.routeId trip.routeDistinction trip.id
        if not (tripsToDelete.Contains gtfsId) then
            activeTripKeys.Add(struct (trip.routeId, trip.routeDistinction, trip.id)) |> ignore
    let activeTripContains routeId distinction tripId =
        activeTripKeys.Contains(struct (routeId, distinction, tripId))
    let activeTripCountsByRoute =
        activeTripKeys
        |> Seq.map (fun struct (routeId, distinction, _) -> routeId, distinction)
        |> Seq.countBy id
        |> Map
    let stopCountries =
        batch.stops
        |> Seq.map (fun stop ->
            let country =
                match stop.country with
                | Some value when not (String.IsNullOrWhiteSpace(value)) ->
                    Some (canonicalCountry value)
                | _ when stop.regionId.IsSome -> Some "CZ"
                | _ -> None
            stop.id, country)
        |> Map
    // Only these routes need expensive trip-call country classification.
    // Unknown stop countries deliberately remain candidates for inspection.
    let potentiallyInternationalRoutes =
        let values = HashSet<struct (string * int)>()
        for route in batch.routes do
            if routeIsDeclaredInternational route then
                values.Add(struct (route.id, route.idDistinction)) |> ignore
        for routeStop in batch.routeStops do
            if stopCountries |> Map.tryFind routeStop.stopId |> Option.flatten <> Some "CZ" then
                values.Add(struct (routeStop.routeId, routeStop.routeDistinction)) |> ignore
        values
    let isPotentialRoute routeId distinction =
        potentiallyInternationalRoutes.Contains(struct (routeId, distinction))
    let activeTripsByRoute =
        activeTripKeys
        |> Seq.choose (fun struct (routeId, distinction, tripId) ->
            if isPotentialRoute routeId distinction then
                Some ((routeId, distinction), (routeId, distinction, tripId))
            else None)
        |> Seq.groupBy fst
        |> Seq.map (fun (routeKey, trips) -> routeKey, trips |> Seq.map snd |> Seq.toArray)
        |> Map
    let callsByTrip = Dictionary<struct (string * int * int64), ResizeArray<JdfModel.TripStop>>()
    for callIndex = 0 to batch.tripStops.Length-1 do
        let call = batch.tripStops.[callIndex]
        if isPotentialRoute call.routeId call.routeDistinction
           && activeTripContains call.routeId call.routeDistinction call.tripId
           && callIsEmitted call then
            let key = struct (call.routeId, call.routeDistinction, call.tripId)
            match callsByTrip.TryGetValue(key) with
            | true, values -> values.Add(call)
            | _ ->
                let values = ResizeArray()
                values.Add(call)
                callsByTrip.[key] <- values
        if (callIndex + 1) % 1_000_000 = 0 then
            progress "international-calls" (int64 (callIndex + 1)) (Some (int64 batch.tripStops.Length))
    progress "international-calls" (int64 batch.tripStops.Length) (Some (int64 batch.tripStops.Length))
    let callsForTrip (routeId, distinction, tripId) =
        match callsByTrip.TryGetValue(struct (routeId, distinction, tripId)) with
        | true, values -> values.ToArray()
        | _ -> [||]
    let integratedRoutes =
        batch.routeIntegrations
        |> Seq.map (fun integration -> integration.routeId, integration.routeDistinction)
        |> Set
    let overridesByRoute =
        overrides
        |> Seq.map (fun item -> (item.routeId, item.routeDistinction), item)
        |> Map
    let adjacentCountries = set ["A"; "D"; "PL"; "SK"]

    let classifyTrip integrated (calls: JdfModel.TripStop array) =
        let countriesWithUnknown =
            calls
            |> Array.map (fun call -> stopCountries |> Map.tryFind call.stopId |> Option.flatten)
        let countries = countriesWithUnknown |> Array.choose id |> Set.ofArray
        let hasCzech = countries.Contains "CZ"
        let foreign = countries.Remove "CZ"
        if foreign.IsEmpty then
            if hasCzech && not (countriesWithUnknown |> Array.exists Option.isNone) then DomesticTrip
            else RejectedCrossBorderTrip("unknown_stop_country", None, None)
        elif not hasCzech then ForeignOnlyTrip
        elif countriesWithUnknown |> Array.exists Option.isNone then
            RejectedCrossBorderTrip("unknown_stop_country", None, None)
        elif not (Set.isSubset foreign adjacentCountries) then
            RejectedCrossBorderTrip("non_adjacent_country", None, None)
        elif calls |> Array.exists (fun call -> call.kilometer.IsNone) then
            RejectedCrossBorderTrip("missing_timetable_kilometres", None, None)
        else
            let czechKm =
                calls
                |> Array.choose (fun call ->
                    if stopCountries.[call.stopId] = Some "CZ" then call.kilometer else None)
            let foreignKm =
                calls
                |> Array.choose (fun call ->
                    if stopCountries.[call.stopId] <> Some "CZ" then call.kilometer else None)
            let allKm = calls |> Array.choose (fun call -> call.kilometer)
            let span = Array.max allKm - Array.min allKm
            let depth =
                foreignKm
                |> Array.map (fun km -> czechKm |> Array.map (fun czech -> abs (km - czech)) |> Array.min)
                |> Array.max
            let spanLimit, depthLimit = if integrated then 200m, 80m else 120m, 60m
            if span > spanLimit then
                RejectedCrossBorderTrip("trip_span_exceeds_limit", Some span, Some depth)
            elif depth > depthLimit then
                RejectedCrossBorderTrip("foreign_depth_exceeds_limit", Some span, Some depth)
            else QualifyingCrossBorderTrip(span, depth)

    let classify (route: JdfModel.Route) =
        let routeKey = route.id, route.idDistinction
        let routeNeedsClassification = isPotentialRoute route.id route.idDistinction
        let routeTrips =
            activeTripsByRoute
            |> Map.tryFind routeKey
            |> Option.defaultValue [||]
            |> Seq.map (fun key -> key, classifyTrip (integratedRoutes.Contains routeKey)
                                            (callsForTrip key))
            |> Seq.toArray
        let calls = routeTrips |> Seq.collect (fun (key, _) -> callsForTrip key) |> Seq.toArray
        let countriesWithUnknown =
            calls
            |> Seq.map (fun call -> stopCountries |> Map.tryFind call.stopId |> Option.flatten)
            |> Seq.toArray
        let countries = countriesWithUnknown |> Array.choose id |> Array.distinct |> Array.sort
        let foreignCountries = countries |> Set.ofArray |> Set.remove "CZ"
        let integrated = integratedRoutes.Contains routeKey
        let isPotentiallyInternational =
            routeNeedsClassification
            && (not foreignCountries.IsEmpty || routeIsDeclaredInternational route)
        let domesticCount =
            if routeNeedsClassification then
                routeTrips |> Array.sumBy (fun (_, value) -> if value = DomesticTrip then 1 else 0)
            else activeTripCountsByRoute |> Map.tryFind routeKey |> Option.defaultValue 0
        let qualifying = routeTrips |> Array.choose (fun (_, value) -> match value with QualifyingCrossBorderTrip(span, depth) -> Some(span, depth) | _ -> None)
        let rejectedCount = routeTrips |> Array.sumBy (fun (_, value) -> match value with RejectedCrossBorderTrip _ -> 1 | _ -> 0)
        let rejectionReasons =
            routeTrips
            |> Array.choose (fun (_, value) -> match value with RejectedCrossBorderTrip(reason, _, _) -> Some reason | _ -> None)
            |> Array.distinct
        let foreignOnlyCount = routeTrips |> Array.sumBy (fun (_, value) -> if value = ForeignOnlyTrip then 1 else 0)
        let maximumSpan = if qualifying.Length = 0 then None else Some (qualifying |> Array.map fst |> Array.max)
        let maximumDepth = if qualifying.Length = 0 then None else Some (qualifying |> Array.map snd |> Array.max)
        let calculatedKeep = not isPotentiallyInternational || qualifying.Length > 0
        let calculatedReason =
            if not isPotentiallyInternational then "domestic"
            elif qualifying.Length > 0 then "regional_adjacent"
            elif foreignCountries.IsEmpty then "international_metadata_without_foreign_geography"
            elif foreignOnlyCount > 0 && rejectedCount = 0 then "foreign_only_trip"
            elif rejectionReasons.Length = 1 then rejectionReasons.[0]
            else "no_qualifying_cross_border_trip"
        let decision =
            match overridesByRoute |> Map.tryFind routeKey with
            | Some (routeOverride: InternationalRouteOverride) ->
                { routeId = route.id; routeDistinction = route.idDistinction
                  keep = routeOverride.decision = KeepRoute
                  reason = $"override: {routeOverride.reason}"
                  countries = countries; maximumTripSpanKm = maximumSpan
                  maximumForeignDepthKm = maximumDepth; integrated = integrated
                  overrideDecision = Some routeOverride.decision
                  retainedDomesticTrips = domesticCount
                  qualifyingCrossBorderTrips = qualifying.Length
                  rejectedCrossBorderTrips = rejectedCount
                  foreignOnlyTrips = foreignOnlyCount }
            | None ->
                { routeId = route.id; routeDistinction = route.idDistinction
                  keep = calculatedKeep; reason = calculatedReason
                  countries = countries; maximumTripSpanKm = maximumSpan
                  maximumForeignDepthKm = maximumDepth; integrated = integrated
                  overrideDecision = None
                  retainedDomesticTrips = domesticCount
                  qualifyingCrossBorderTrips = qualifying.Length
                  rejectedCrossBorderTrips = rejectedCount
                  foreignOnlyTrips = foreignOnlyCount }
        routeKey, routeTrips, decision

    let classifications = Array.zeroCreate batch.routes.Length
    let mutable classifiedRoutes = 0L
    let classificationProgressLock = obj()
    let mutable lastClassificationProgress = 0L
    let classifyAt index =
        classifications.[index] <- classify batch.routes.[index]
        let count = Interlocked.Increment(&classifiedRoutes)
        if count % 250L = 0L || count = int64 batch.routes.Length then
            lock classificationProgressLock (fun () ->
                if count > lastClassificationProgress then
                    lastClassificationProgress <- count
                    progress "international-routes" count (Some (int64 batch.routes.Length)))
    if maximumWorkers <= 1 || batch.routes.Length <= 1 then
        for index = 0 to batch.routes.Length-1 do
            classifyAt index
    else
        let parallelOptions = ParallelOptions(MaxDegreeOfParallelism = maximumWorkers)
        Parallel.For(0, batch.routes.Length, parallelOptions, classifyAt)
        |> ignore
    let decisions = classifications |> Array.map (fun (_, _, decision) -> decision)
    let decisionsByRoute =
        decisions
        |> Seq.map (fun decision -> (decision.routeId, decision.routeDistinction), decision)
        |> Map
    let keptRouteKeys =
        decisions
        |> Seq.filter (fun decision -> decision.keep)
        |> Seq.map (fun decision -> decision.routeId, decision.routeDistinction)
        |> Set
    let routeKept routeId routeDistinction = keptRouteKeys.Contains(routeId, routeDistinction)
    let dispositionByTrip = classifications |> Seq.collect (fun (_, trips, _) -> trips) |> Map
    let overridesByKeptRoute =
        decisions |> Seq.choose (fun d -> d.overrideDecision |> Option.map (fun value -> (d.routeId, d.routeDistinction), value)) |> Map
    let tripAllowed (trip: JdfModel.Trip) =
        let routeKey = trip.routeId, trip.routeDistinction
        if not (routeKept trip.routeId trip.routeDistinction) then false
        elif not (activeTripContains trip.routeId trip.routeDistinction trip.id) then true
        elif not (isPotentialRoute trip.routeId trip.routeDistinction) then true
        else
            match overridesByKeptRoute |> Map.tryFind routeKey, dispositionByTrip.[trip.routeId, trip.routeDistinction, trip.id] with
            | Some KeepRoute, ForeignOnlyTrip -> false
            | Some KeepRoute, _ -> true
            | _, DomesticTrip | _, QualifyingCrossBorderTrip _ -> true
            | _ -> false
    let keptTrips = batch.trips |> Array.filter tripAllowed
    let keptTripKeys = HashSet<struct (string * int * int64)>()
    for trip in keptTrips do
        keptTripKeys.Add(struct (trip.routeId, trip.routeDistinction, trip.id)) |> ignore
    let tripKept routeId routeDistinction tripId =
        keptTripKeys.Contains(struct (routeId, routeDistinction, tripId))
    let routeStops =
        batch.routeStops
        |> Array.filter (fun stop -> routeKept stop.routeId stop.routeDistinction)
    let retainedStopIds = routeStops |> Seq.map (fun stop -> stop.stopId) |> Set
    let agencyAlternations =
        batch.agencyAlternations
        |> Array.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
    let retainedAgencies =
        seq {
            yield! batch.routes
                   |> Seq.filter (fun route -> routeKept route.id route.idDistinction)
                   |> Seq.map (fun route -> route.agencyId, route.agencyDistinction)
            yield! agencyAlternations
                   |> Seq.map (fun value -> value.agencyId, value.agencyDistinction)
        }
        |> Set
    let usedTripGroups = keptTrips |> Seq.choose (fun trip -> trip.tripGroupId) |> Set
    let filteredBatch = {
        batch with
            stops = batch.stops |> Array.filter (fun stop -> retainedStopIds.Contains stop.id)
            stopPosts = batch.stopPosts |> Array.filter (fun stop -> retainedStopIds.Contains stop.stopId)
            agencies = batch.agencies |> Array.filter (fun agency -> retainedAgencies.Contains(agency.id, agency.idDistinction))
            routes =
                batch.routes
                |> Array.filter (fun route -> routeKept route.id route.idDistinction)
                |> Array.map (fun route ->
                    let decision = decisionsByRoute.[route.id, route.idDistinction]
                    if decision.qualifyingCrossBorderTrips > 0 then
                        { route with routeType = JdfModel.RegionalInternational }
                    else route)
            routeIntegrations = batch.routeIntegrations |> Array.filter (fun value -> routeKept value.routeId value.routeDistinction)
            routeStops = routeStops
            trips = keptTrips
            tripGroups = batch.tripGroups |> Array.filter (fun group -> usedTripGroups.Contains group.id)
            tripStops = batch.tripStops |> Array.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
            routeInfo = batch.routeInfo |> Array.filter (fun value -> routeKept value.routeId value.routeDistinction)
            serviceNotes = batch.serviceNotes |> Array.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
            transfers = batch.transfers |> Array.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
            agencyAlternations = agencyAlternations
            alternateRouteNames = batch.alternateRouteNames |> Array.filter (fun value -> routeKept value.routeId value.routeDistinction)
            reservationOptions = batch.reservationOptions |> Array.filter (fun value -> tripKept value.routeId value.routeDistinction value.tripId)
            stopLocations = batch.stopLocations |> Array.filter (fun value -> retainedStopIds.Contains value.stopId)
            stopLocationSources = batch.stopLocationSources |> Array.filter (fun value -> retainedStopIds.Contains value.stopId)
            postCandidateEvidence =
                batch.postCandidateEvidence
                |> Array.filter (fun value -> retainedStopIds.Contains value.stopId)
            routingDemands =
                batch.routingDemands
                |> Array.filter (fun value ->
                    value.previousStopId
                    |> Option.map retainedStopIds.Contains
                    |> Option.defaultValue true
                    && (value.nextStopId
                        |> Option.map retainedStopIds.Contains
                        |> Option.defaultValue true))
        }
    { batch = filteredBatch; decisions = decisions }

let applyInternationalRoutePolicyWithCalendar
        (policy: InternationalRoutePolicy)
        (overrides: InternationalRouteOverride array)
        (calendar: CalendarPreparation)
        (batch: JdfModel.JdfBatch) =
    applyInternationalRoutePolicyInternal 1 (fun _ _ _ -> ()) calendar.tripsToDelete policy overrides batch

let applyInternationalRoutePolicyWithCalendarAndWorkers
        maximumWorkers
        (policy: InternationalRoutePolicy)
        (overrides: InternationalRouteOverride array)
        (calendar: CalendarPreparation)
        (batch: JdfModel.JdfBatch) =
    if maximumWorkers <= 0 then invalidArg "maximumWorkers" "Worker count must be positive"
    applyInternationalRoutePolicyInternal maximumWorkers (fun _ _ _ -> ()) calendar.tripsToDelete policy overrides batch

let applyInternationalRoutePolicyWithCalendarWorkersAndProgress
        maximumWorkers
        (progress: string -> int64 -> int64 option -> unit)
        (policy: InternationalRoutePolicy)
        (overrides: InternationalRouteOverride array)
        (calendar: CalendarPreparation)
        (batch: JdfModel.JdfBatch) =
    if maximumWorkers <= 0 then invalidArg "maximumWorkers" "Worker count must be positive"
    applyInternationalRoutePolicyInternal maximumWorkers progress calendar.tripsToDelete policy overrides batch

let applyInternationalRoutePolicy (policy: InternationalRoutePolicy)
                                  (overrides: InternationalRouteOverride array)
                                  (batch: JdfModel.JdfBatch) =
    let calendar = prepareGtfsCalendar batch
    applyInternationalRoutePolicyWithCalendar policy overrides calendar batch

let logInternationalRouteDecisions (policy: InternationalRoutePolicy)
                                   (decisions: InternationalRouteDecision array) =
    if policy <> KeepAll then
        let crossBorder = decisions |> Seq.filter (fun value -> value.countries |> Array.exists ((<>) "CZ"))
        let retained = crossBorder |> Seq.filter (fun value -> value.keep) |> Seq.length
        let dropped = crossBorder |> Seq.filter (fun value -> not value.keep) |> Seq.length
        Log.Information(
            "International route policy {Policy}: retained {RetainedRoutes} and dropped {DroppedRoutes} cross-border route distinctions",
            internationalRoutePolicyName policy, retained, dropped)

let getGtfsTrips (jdfBatch: JdfModel.JdfBatch) =
    let lastStopPerTrip = Dictionary<struct (string * int * int64), struct (int64 * int64)>()
    for call in jdfBatch.tripStops do
        let key = struct (call.routeId, call.routeDistinction, call.tripId)
        let order = call.routeStopId * (if Jdf.tripIsReverse call.tripId then -1L else 1L)
        match lastStopPerTrip.TryGetValue(key) with
        | true, struct (oldOrder, _) when oldOrder >= order -> ()
        | _ -> lastStopPerTrip.[key] <- struct (order, call.stopId)
    let stopById = jdfBatch.stops |> Seq.map (fun s -> s.id, s) |> Map
    jdfBatch.trips
    |> Seq.map (fun jdfTrip ->
        let id = jdfTripId jdfTrip.routeId jdfTrip.routeDistinction jdfTrip.id
        let attrs = Jdf.parseAttributes jdfBatch jdfTrip.attributes
        let wheelchairAccessible =
                attrs |> Set.contains JdfModel.WheelchairAccessible
                || attrs |> Set.contains JdfModel.PartlyWheelchairAccessible
        ({
            routeId = jdfRouteId jdfTrip.routeId jdfTrip.routeDistinction
            serviceId = id
            id = id
            headsign =
                let struct (_, stopId) =
                    lastStopPerTrip.[struct (jdfTrip.routeId, jdfTrip.routeDistinction, jdfTrip.id)]
                stopById.[stopId]
                |> getStopName
                |> Some
            // This is kind of arbitrary
            // TODO: Customizability?
            shortName = Some (sprintf "%s %d" jdfTrip.routeId jdfTrip.id)
            directionId = Some (if Jdf.tripIsReverse jdfTrip.id
                                then "0" else "1")
            blockId = None
            shapeId = None
            wheelchairAccessible =
                Some (if wheelchairAccessible then "1" else "0")
            bikesAllowed =
                Some (if attrs |> Set.contains JdfModel.BicycleTransport
                      then GtfsModel.OneOrMore
                      else GtfsModel.NoBicycles)
        }: GtfsModel.Trip))

[<Flags>]
type private StopTimeAttributeFlags =
    | NoStopTimeFlags = 0
    | RequestStopFlag = 1
    | ConditionalServiceFlag = 2
    | CommissionServiceFlag = 4
    | ExitOnlyFlag = 8
    | BoardingOnlyFlag = 16

type StreamingStopTimeRow = {
    stopTime: GtfsModel.StopTime
    sourceRouteStopId: int64
}

let private getGtfsStopTimeRowsInternal adjacentTripGroups stopIdCis
                                        (postPlan: PostEstimationPlan)
                                        (jdfBatch: JdfModel.JdfBatch) =
    let periods = Dictionary<int64, Period>()
    let periodOptions = Dictionary<int64, Period option>()
    let periodForSeconds seconds =
        match periods.TryGetValue(seconds) with
        | true, value -> value
        | _ ->
            let value = Period.FromSeconds(seconds)
            periods.[seconds] <- value
            value

    let periodOptionForSeconds seconds =
        match periodOptions.TryGetValue(seconds) with
        | true, value -> value
        | _ ->
            let value = Some (periodForSeconds seconds)
            periodOptions.[seconds] <- value
            value

    // F# options are reference objects. Reusing these values avoids retaining
    // several fresh option wrappers for every stop-time row in national feeds.
    let noService = Some GtfsModel.NoService
    let regularService = Some GtfsModel.RegularlyScheduled
    let coordinationWithDriver = Some GtfsModel.CoordinationWithDriver
    let phoneBefore = Some GtfsModel.PhoneBefore
    let exactTimepoint = Some GtfsModel.Exact

    let unspecifiedStopIds = Dictionary<int64, string>()
    let stopPostIds = Dictionary<struct (int64 * int64), string>()
    let stopPostNumberIds = Dictionary<struct (int64 * string), string>()
    let convertedStopId (inferredSelection: DerivedPostSelection option)
                        (call: JdfModel.TripStop) =
        match call.stopPostId, call.stopPostNum |> Option.bind nonEmptyTrimmed with
        | Some stopPostId, _ ->
            let key = struct (call.stopId, stopPostId)
            match stopPostIds.TryGetValue(key) with
            | true, value -> value
            | _ ->
                let value = jdfStopPostId stopIdCis call.stopId stopPostId
                stopPostIds.[key] <- value
                value
        | None, Some stopPostNumber ->
            let key = struct (call.stopId, stopPostNumber)
            match stopPostNumberIds.TryGetValue(key) with
            | true, value -> value
            | _ ->
                let value = jdfStopPostNumId stopIdCis call.stopId stopPostNumber
                stopPostNumberIds.[key] <- value
                value
        | None, None ->
            match inferredSelection with
            | Some selection -> inferredPostId stopIdCis postPlan selection
            | None ->
                match unspecifiedStopIds.TryGetValue(call.stopId) with
                | true, value -> value
                | _ ->
                    let value = jdfUnspecifiedStopId stopIdCis call.stopId
                    unspecifiedStopIds.[call.stopId] <- value
                    value

    let attributeFlagsById = Dictionary<int, StopTimeAttributeFlags>()
    for attribute in jdfBatch.attributeRefs do
        let flag =
            match attribute.value with
            | JdfModel.RequestStop -> StopTimeAttributeFlags.RequestStopFlag
            | JdfModel.ConditionalService -> StopTimeAttributeFlags.ConditionalServiceFlag
            | JdfModel.CommisionServiceOnly -> StopTimeAttributeFlags.CommissionServiceFlag
            | JdfModel.ExitOnly -> StopTimeAttributeFlags.ExitOnlyFlag
            | JdfModel.BoardingOnly -> StopTimeAttributeFlags.BoardingOnlyFlag
            | _ -> StopTimeAttributeFlags.NoStopTimeFlags
        if flag <> StopTimeAttributeFlags.NoStopTimeFlags then
            attributeFlagsById.[attribute.attributeId] <- flag

    let flagsForAttributes (attributes: int option array) =
        let mutable flags = StopTimeAttributeFlags.NoStopTimeFlags
        for attributeId in attributes do
            match attributeId with
            | Some id ->
                match attributeFlagsById.TryGetValue(id) with
                | true, value -> flags <- flags ||| value
                | _ -> ()
            | None -> ()
        flags

    let stopFlagsById = Dictionary<int64, StopTimeAttributeFlags>()
    for stop in jdfBatch.stops do
        stopFlagsById.[stop.id] <- flagsForAttributes stop.attributes

    let preciseStopIds =
        jdfBatch.stopLocations
        |> Seq.choose (fun location ->
            if location.precision = JdfModel.StopPrecise then Some location.stopId else None)
        |> HashSet
    let modesByRoute = Dictionary<struct (string * int), JdfModel.TransportMode>()
    for route in jdfBatch.routes do
        modesByRoute.[struct (route.id, route.idDistinction)] <- route.transportMode

    let inferredSelectionsForTrip routeId routeDistinction mode
                                      (orderedCalls: JdfModel.TripStop array) =
        let result = Dictionary<int64, DerivedPostSelection>()
        if postPlan.calls.Count > 0 then
            let usable = orderedCalls |> Array.filter callIsUsable
            let patternHash =
                if usable.Length = 0 then completePatternHash usable
                else
                    match postPlan.tripPatternHashes.TryGetValue(
                              struct (routeId, routeDistinction, usable.[0].tripId)) with
                    | true, value -> value
                    | _ -> completePatternHash usable
            let direction = if usable.Length > 0 && Jdf.tripIsReverse usable.[0].tripId then 1 else 0
            let mutable start = 0
            while start < usable.Length do
                let stopId = usable.[start].stopId
                let mutable finish = start + 1
                while finish < usable.Length && usable.[finish].stopId = stopId do
                    finish <- finish + 1
                let blockLength = finish - start
                let previousStopId =
                    seq { start - 1 .. -1 .. 0 }
                    |> Seq.tryPick (fun index ->
                        let candidate = usable.[index].stopId
                        if candidate <> stopId && preciseStopIds.Contains(candidate)
                        then Some candidate else None)
                let nextStopId =
                    seq { finish .. usable.Length - 1 }
                    |> Seq.tryPick (fun index ->
                        let candidate = usable.[index].stopId
                        if candidate <> stopId && preciseStopIds.Contains(candidate)
                        then Some candidate else None)
                for index = start to finish - 1 do
                    let call = usable.[index]
                    if authoredPostKey call |> Option.isNone then
                        let previous, next, role =
                            if blockLength = 1 then previousStopId, nextStopId, "through"
                            elif index = start then previousStopId, None, "incoming"
                            elif index = finish - 1 then None, nextStopId, "outgoing"
                            else None, None, "interior"
                        let contextKey = {
                            stopId = stopId; mode = mode; lineId = routeId
                            routeDistinction = routeDistinction; direction = direction
                            patternHash = patternHash; position = index
                            sameStopBlockRole = role
                        }
                        match postPlan.calls.TryGetValue(contextKey) with
                        | true, selection -> result.[call.routeStopId] <- selection
                        | _ -> ()
                start <- finish
        result

    let tripKey (call: JdfModel.TripStop) =
        call.routeId, call.routeDistinction, call.tripId
    let tripStopGroups =
        if adjacentTripGroups then
            let calls = jdfBatch.tripStops
            let spans = ResizeArray<struct (string * int * int64 * int * int * string)>()
            let mutable startIndex = 0
            while startIndex < calls.Length do
                let first = calls.[startIndex]
                let mutable endIndex = startIndex + 1
                while endIndex < calls.Length
                      && calls.[endIndex].routeId = first.routeId
                      && calls.[endIndex].routeDistinction = first.routeDistinction
                      && calls.[endIndex].tripId = first.tripId do
                    endIndex <- endIndex + 1
                spans.Add(struct (
                    first.routeId, first.routeDistinction, first.tripId,
                    startIndex, endIndex-startIndex,
                    jdfTripId first.routeId first.routeDistinction first.tripId))
                startIndex <- endIndex
            spans.Sort(Comparer<struct (string * int * int64 * int * int * string)>.Create(
                fun struct (leftRoute,leftDistinction,leftTrip,_,_,leftId)
                    struct (rightRoute,rightDistinction,rightTrip,_,_,rightId) ->
                    let byId = StringComparer.Ordinal.Compare(leftId,rightId)
                    if byId <> 0 then byId
                    else compare (leftRoute,leftDistinction,leftTrip)
                                 (rightRoute,rightDistinction,rightTrip)))
            spans
            |> Seq.map (fun struct (routeId,distinction,tripId,start,count,_) ->
                let tripCalls = Array.zeroCreate<JdfModel.TripStop> count
                Array.Copy(calls,start,tripCalls,0,count)
                (routeId,distinction,tripId),tripCalls)
        else
            jdfBatch.tripStops
            |> Seq.groupBy tripKey
            |> Seq.map (fun (key, calls) -> key, calls |> Seq.toArray)

    tripStopGroups
    // We have to deal with stop times for each trip separately,
    // because we have to count 23:59 -> 00:00 crossings
    // to even attempt to comply with GTFS and distinquish days
    // Not even this is enough, though. Imagine a trip that sets out
    // at 8:00 and, without any intermediate stops, arrives at 9:00
    // the next day.
    |> Seq.collect (fun ((routeId, routeDistinction, tripId), jdfTripStops) ->
        assert (jdfTripStops.Length >= 2)
        let isReverseTrip = jdfTripStops.[0].tripId % 2L = 0L
        let gtfsTripId = jdfTripId routeId routeDistinction tripId
        let orderedCalls =
            jdfTripStops
            |> Array.sortBy (fun call ->
                call.routeStopId * (if isReverseTrip then -1L else 1L))
        let inferredSelections =
            inferredSelectionsForTrip
                routeId routeDistinction
                modesByRoute.[struct (routeId, routeDistinction)] orderedCalls

        let mutable lastTimeDT: LocalTime option = None
        let mutable dayOffsetSeconds = 0L

        orderedCalls
        |> Seq.mapi (fun i jdfTripStop ->
            match jdfTripStop.departureTime with
            | Some JdfModel.Passing | Some JdfModel.NotPassing -> None
            // The JDF specification allows stops that aren't served to have a
            // blank arrival and departure time. In GTFS, such stops are just
            // omitted.
            | None when jdfTripStop.arrivalTime = None -> None
            | _ ->
                let tripStopTimeExtract tst =
                    tst
                    |> Option.map (fun x ->
                        match x with
                        | JdfModel.StopTime dt -> dt
                        | _ -> failwith "Invalid data"
                    )

                let adjustTime dtOpt =
                    match dtOpt with
                    | None -> None
                    | Some dt ->
                        match lastTimeDT with
                        | Some lt ->
                            if lt > dt
                            then dayOffsetSeconds <- dayOffsetSeconds + 86400L
                        | None -> ()
                        lastTimeDT <- Some dt
                        let seconds =
                            int64 (dt.Hour * 3600 + dt.Minute * 60 + dt.Second)
                            + dayOffsetSeconds
                        periodOptionForSeconds seconds

                let arrTime =
                    tripStopTimeExtract jdfTripStop.arrivalTime
                    |> adjustTime
                let depTime =
                    tripStopTimeExtract jdfTripStop.departureTime
                    |> adjustTime

                let combinedFlags =
                    stopFlagsById.[jdfTripStop.stopId]
                    ||| flagsForAttributes jdfTripStop.attributes
                let hasFlag flag = (combinedFlags &&& flag) <> StopTimeAttributeFlags.NoStopTimeFlags

                let service =
                    if hasFlag StopTimeAttributeFlags.RequestStopFlag then
                        coordinationWithDriver
                    else if hasFlag StopTimeAttributeFlags.ConditionalServiceFlag then
                    // "ConditionalService" is a very broad attribute
                    // which basically says "look at the description to
                    // find out". GTFS doesn't have such an option,
                    // and PhoneBefore implies some human interaction.
                    // so that's my choice.
                        phoneBefore
                    else if hasFlag StopTimeAttributeFlags.CommissionServiceFlag then
                        phoneBefore
                    else
                        regularService

                let stopTime: GtfsModel.StopTime = {
                    tripId = gtfsTripId
                    arrivalTime = arrTime |> Option.orElse depTime
                    departureTime = depTime |> Option.orElse arrTime
                    stopId =
                        match inferredSelections.TryGetValue(jdfTripStop.routeStopId) with
                        | true, selection -> convertedStopId (Some selection) jdfTripStop
                        | _ -> convertedStopId None jdfTripStop
                    stopSequence = i
                    headsign = None
                    pickupType =
                        if hasFlag StopTimeAttributeFlags.ExitOnlyFlag
                        then noService
                        else service
                    dropoffType =
                        if hasFlag StopTimeAttributeFlags.BoardingOnlyFlag
                        then noService
                        else service
                    shapeDistTraveled = jdfTripStop.kilometer
                    // This will be dynamic when support for JDF's
                    // min/max times comes.
                    timepoint = exactTimepoint
                    stopZoneIds = None
                }
                Some { stopTime = stopTime; sourceRouteStopId = jdfTripStop.routeStopId }
        )
        |> Seq.choose id
    )

let private getGtfsStopTimesInternal adjacentTripGroups stopIdCis postPlan jdfBatch =
    getGtfsStopTimeRowsInternal adjacentTripGroups stopIdCis postPlan jdfBatch
    |> Seq.map (fun value -> value.stopTime)

// Standalone conversion preserves support for unusual JDF files whose trip
// calls are not contiguous. The merged national bundle has canonical adjacent
// trip groups and uses the bounded implementation directly.
let getGtfsStopTimes stopIdCis (jdfBatch: JdfModel.JdfBatch) =
    getGtfsStopTimesInternal false stopIdCis (buildPostEstimationPlan jdfBatch) jdfBatch

let getCzRoutes (publicLineNumbers: Map<string * int, string option>)
                (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.routes
    |> Array.map (fun route ->
        {
            routeId = jdfRouteId route.id route.idDistinction
            cisLineId = Some route.id
            publicLineNumber =
                publicLineNumbers.[route.id, route.idDistinction]
            sourceProvenance = sprintf "jdf:%s" jdfBatch.version.version
        }: GtfsModel.CzRoute)

let getCzTrips (tripsToDelete: Set<string>)
               (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.trips
    |> Seq.choose (fun trip ->
        let sourceTripId =
            jdfTripId trip.routeId trip.routeDistinction trip.id
        if tripsToDelete |> Set.contains sourceTripId then None
        else
            Some ({
                tripId = sourceTripId
                cisLineId = Some trip.routeId
                cisTripId = Some trip.id
                trainNumber = None
                sourceTripIds = Some sourceTripId
                coverageSources = Some "jdf"
            }: GtfsModel.CzTrip))
    |> Seq.toArray

let private getCzStopsWithPlan stopIdsCis (plan: PostEstimationPlan)
                               (jdfBatch: JdfModel.JdfBatch) =
    let cisStopId stopId = if stopIdsCis then Some stopId else None
    let sourceStopId stopId = sprintf "jdf:stop:%d" stopId
    let sourcePostId stopId stopPostId =
        sprintf "jdf:stop:%d:post:id:%d" stopId stopPostId
    let sourcePostNum (stopId: int64) (stopPostNum: string) =
        sprintf "jdf:stop:%d:post:%s"
                stopId
                (Uri.EscapeDataString(stopPostNum))
    let postNumbersById = stopPostNumbersById jdfBatch

    let stops =
        jdfBatch.stops
        |> Array.map (fun stop ->
            let stopId = jdfStopId stopIdsCis stop.id
            {
                stopId = stopId
                stopPlaceId = stopId
                cisStopId = cisStopId stop.id
                postId = None
                aswId = None
                sourceIds = Some (sourceStopId stop.id)
            }: GtfsModel.CzStop)
    let unspecifiedStops =
        jdfBatch.stops
        |> Array.map (fun stop ->
            {
                stopId = jdfUnspecifiedStopId stopIdsCis stop.id
                stopPlaceId = jdfStopId stopIdsCis stop.id
                cisStopId = cisStopId stop.id
                postId = None
                aswId = None
                sourceIds = None
            }: GtfsModel.CzStop)
    let stopPosts =
        jdfBatch.stopPosts
        |> Array.map (fun stopPost ->
            let sourceIds =
                match postNumbersById
                      |> Map.tryFind (stopPost.stopId, stopPost.stopPostId) with
                | Some stopPostNum ->
                    String.Join(",", [|
                        sourcePostId stopPost.stopId stopPost.stopPostId
                        sourcePostNum stopPost.stopId stopPostNum
                    |])
                | None -> sourcePostId stopPost.stopId stopPost.stopPostId
            {
                stopId = jdfStopPostId stopIdsCis
                                           stopPost.stopId
                                           stopPost.stopPostId
                stopPlaceId = jdfStopId stopIdsCis stopPost.stopId
                cisStopId = cisStopId stopPost.stopId
                postId = Some (string stopPost.stopPostId)
                aswId = None
                sourceIds = Some sourceIds
            }: GtfsModel.CzStop)
    let numberedStopPosts =
        derivedStopPosts jdfBatch
        |> Array.map (fun (stopId, stopPostNum) ->
            {
                stopId = jdfStopPostNumId stopIdsCis stopId stopPostNum
                stopPlaceId = jdfStopId stopIdsCis stopId
                cisStopId = cisStopId stopId
                postId = Some stopPostNum
                aswId = None
                sourceIds = Some (sourcePostNum stopId stopPostNum)
            }: GtfsModel.CzStop)
    let inferredPosts =
        plan.inferredLocations
        |> Seq.sortBy (fun value -> value.stopId, value.locationId)
        |> Seq.map (fun (selection: DerivedPostSelection) ->
            let inferredCisStopId = cisStopId selection.stopId
            ({
                stopId = inferredPostId stopIdsCis plan selection
                stopPlaceId = jdfStopId stopIdsCis selection.stopId
                cisStopId = inferredCisStopId
                postId = None
                aswId = None
                sourceIds = None
            }: GtfsModel.CzStop))
        |> Seq.toArray
    Array.concat [stops; unspecifiedStops; stopPosts; numberedStopPosts; inferredPosts]

let getCzStops stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
    getCzStopsWithPlan stopIdsCis (buildPostEstimationPlan jdfBatch) jdfBatch

let getCzStopZones stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
    jdfBatch.routeStops
    |> Seq.collect (fun routeStop ->
        Jdf.normalizeZoneTokens [routeStop.zone]
        |> Seq.map (fun zoneCode ->
            {
                stopPlaceId = jdfStopId stopIdsCis routeStop.stopId
                zoneId = jdfSourceZoneId routeStop.routeId
                                             routeStop.routeDistinction
                                             zoneCode
                zoneCode = zoneCode
                routeId = jdfRouteId routeStop.routeId
                                     routeStop.routeDistinction
                idsSystemId = None
                sourceProvenance = sprintf "jdf:%s" jdfBatch.version.version
            }: GtfsModel.CzStopZone))
    |> Seq.distinct
    |> Seq.sortBy (fun zone -> zone.stopPlaceId, zone.routeId, zone.zoneId)
    |> Seq.toArray

let warnUnhandledServiceNotes (jdfBatch: JdfModel.JdfBatch) () =
    jdfBatch.serviceNotes
    |> Seq.filter (fun sn -> sn.noteType = None)
    |> Seq.iter (fun sn ->
        Log.Warning("Unhandled JDF ServiceNote {Designation} {Note}", sn.designation, sn.note))

let private feedInfo (jdfVersion: JdfModel.JdfVersion)
                     (calendar: GtfsModel.CalendarEntry array)
                     (calendarExceptions: GtfsModel.CalendarException array) =
    let addedDates =
        calendarExceptions
        |> Array.choose (fun exceptionDate ->
            if exceptionDate.exceptionType = GtfsModel.ServiceAdded
            then Some exceptionDate.date else None)
    let starts =
        Array.append (calendar |> Array.map (fun entry -> entry.startDate)) addedDates
    let ends =
        Array.append (calendar |> Array.map (fun entry -> entry.endDate)) addedDates
    let startDate = if starts.Length = 0 then None else Some (Array.min starts)
    let endDate = if ends.Length = 0 then None else Some (Array.max ends)
    let versionParts = [
        Some (sprintf "jdf-%s" jdfVersion.version)
        jdfVersion.batchId |> Option.filter (String.IsNullOrWhiteSpace >> not)
        jdfVersion.creationDate
        |> Option.map (fun date ->
            sprintf "%04d%02d%02d" date.Year date.Month date.Day)
    ]
    let version = versionParts |> List.choose id |> String.concat ":" |> Some
    Gtfs.obehyFeedInfo version startDate endDate

let private assembleGtfsFeed stopIdsCis (jdfBatch: JdfModel.JdfBatch)
                             (postPlan: PostEstimationPlan)
                             tripsToDelete calendar calendarExceptions publicLineNumbers
                             (referencedStopIds: Set<string>) stopTimes =
    let allStops = getGtfsStopsWithPlan stopIdsCis postPlan jdfBatch
    let requiredParentIds =
        allStops
        |> Seq.filter (fun stop -> referencedStopIds.Contains stop.id)
        |> Seq.choose (fun stop -> stop.parentStation)
        |> Set
    let retainedStopIds = Set.union referencedStopIds requiredParentIds
    let stops = allStops |> Array.filter (fun stop -> retainedStopIds.Contains stop.id)
    let feed: GtfsModel.GtfsFeed = {
        agencies = jdfBatch.agencies |> Array.map convertToGtfsAgency
        stops = stops
        routes = getGtfsRoutesWithPublicLines publicLineNumbers jdfBatch
        trips = getGtfsTrips jdfBatch
            |> Seq.filter (fun t ->
                tripsToDelete |> Set.contains t.id |> not)
            |> Seq.toArray
        stopTimes = stopTimes
        calendar = Some calendar
        calendarExceptions = Some calendarExceptions
        feedInfo = Some (feedInfo jdfBatch.version calendar calendarExceptions)
        transfers = None
        czRoutes = Some (getCzRoutes publicLineNumbers jdfBatch)
        czTrips = Some (getCzTrips tripsToDelete jdfBatch)
        czStops =
            getCzStopsWithPlan stopIdsCis postPlan jdfBatch
            |> Array.filter (fun stop -> retainedStopIds.Contains stop.stopId)
            |> Some
        czStopZones =
            getCzStopZones stopIdsCis jdfBatch
            |> Array.filter (fun zone -> retainedStopIds.Contains zone.stopPlaceId)
            |> Some
        czTripStopZones = None
    }
    feed

// Some JDF feeds have only local IDs for stops, some have global IDs for the
// whole CIS. Set stopIdsCis accordingly.
let private getGtfsFeedInternal warnUnhandledNotes adjacentTripGroups stopIdsCis
                                (jdfBatch: JdfModel.JdfBatch) =
    if warnUnhandledNotes then warnUnhandledServiceNotes jdfBatch ()

    let tripsToDelete, calendar, calendarExceptions = getGtfsCalendar jdfBatch
    let publicLineNumbers = getPublicLineNumbers jdfBatch
    let postPlan = emptyPostEstimationPlan
    let stopTimes =
        getGtfsStopTimesInternal adjacentTripGroups stopIdsCis postPlan jdfBatch
        |> Seq.filter (fun ts ->
            tripsToDelete |> Set.contains ts.tripId |> not)
        |> Seq.toArray
    let referencedStopIds = stopTimes |> Seq.map (fun stopTime -> stopTime.stopId) |> Set
    assembleGtfsFeed stopIdsCis jdfBatch postPlan tripsToDelete calendar calendarExceptions
                     publicLineNumbers referencedStopIds stopTimes

type StreamingFeedPreparation = {
    adjacentTripGroups: bool
    stopIdsCis: bool
    batch: JdfModel.JdfBatch
    tripsToDelete: Set<string>
    calendar: GtfsModel.CalendarEntry array
    calendarExceptions: GtfsModel.CalendarException array
    publicLineNumbers: Map<string * int, string option>
    postPlan: PostEstimationPlan
}

let private prepareGtfsFeedForStreamingInternal
    warnUnhandledNotes adjacentTripGroups stopIdsCis
    (calendarPreparation: CalendarPreparation option) (batch: JdfModel.JdfBatch) =
    if warnUnhandledNotes then warnUnhandledServiceNotes batch ()
    let calendarPreparation =
        calendarPreparation |> Option.defaultWith (fun () -> prepareGtfsCalendar batch)
    {
        adjacentTripGroups = adjacentTripGroups
        stopIdsCis = stopIdsCis
        batch = batch
        tripsToDelete = calendarPreparation.tripsToDelete
        calendar = calendarPreparation.calendar
        calendarExceptions = calendarPreparation.calendarExceptions
        publicLineNumbers = getPublicLineNumbers batch
        postPlan = emptyPostEstimationPlan
    }

let prepareGtfsFeedForStreaming warnUnhandledNotes stopIdsCis batch =
    prepareGtfsFeedForStreamingInternal warnUnhandledNotes false stopIdsCis None batch

let prepareGtfsFeedForStreamingBundle stopIdsCis batch =
    prepareGtfsFeedForStreamingInternal false true stopIdsCis None batch

let prepareGtfsFeedForStreamingBundleWithCalendar stopIdsCis calendar batch =
    prepareGtfsFeedForStreamingInternal false true stopIdsCis (Some calendar) batch

// Evidence replay has already performed every post-inference decision.  Keep
// construction of the ordinary GTFS preparation separate so a replay-backed
// conversion cannot accidentally invoke the legacy geometry estimator while
// replacing its result afterwards.
let prepareGtfsFeedForStreamingBundleWithCalendarAndPostPlan
        stopIdsCis (calendar:CalendarPreparation) (postPlan:PostEstimationPlan) batch =
    JdfPostInferencePolicy.PostInferencePhaseProbe.record "gtfs-conversion"
    {
        adjacentTripGroups = true
        stopIdsCis = stopIdsCis
        batch = batch
        tripsToDelete = calendar.tripsToDelete
        calendar = calendar.calendar
        calendarExceptions = calendar.calendarExceptions
        publicLineNumbers = getPublicLineNumbers batch
        postPlan = postPlan
    }

let getStreamingBundleStopTimes preparation =
    getGtfsStopTimesInternal
        preparation.adjacentTripGroups preparation.stopIdsCis preparation.postPlan preparation.batch
    |> Seq.filter (fun stopTime ->
        not (preparation.tripsToDelete.Contains stopTime.tripId))

let getStreamingBundleStopTimeRows preparation =
    getGtfsStopTimeRowsInternal
        preparation.adjacentTripGroups preparation.stopIdsCis preparation.postPlan preparation.batch
    |> Seq.filter (fun value ->
        not (preparation.tripsToDelete.Contains value.stopTime.tripId))

let finishStreamingBundleFeed preparation (referencedStopIds: Set<string>) =
    assembleGtfsFeed
        preparation.stopIdsCis preparation.batch preparation.postPlan preparation.tripsToDelete
        preparation.calendar preparation.calendarExceptions preparation.publicLineNumbers
        referencedStopIds [||]

let getGtfsFeed stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
    getGtfsFeedInternal true false stopIdsCis jdfBatch

// Bundle sidecars retain otherwise-unhandled textual service notes, so the
// standalone conversion warning would be misleading while building a bundle.
let getGtfsFeedForBundle stopIdsCis (jdfBatch: JdfModel.JdfBatch) =
    getGtfsFeedInternal false true stopIdsCis jdfBatch
