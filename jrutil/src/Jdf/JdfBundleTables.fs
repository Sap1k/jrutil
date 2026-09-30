// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Normalized JDF source tables (Parquet sidecars).
module JrUtil.JdfBundleTables

open System
open JrUtil.Hashing
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open System.IO.Compression
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open System.Threading.Tasks
open NodaTime
open Parquet
open Parquet.Schema
open Parquet.Serialization
open Serilog
open JrUtil
open JrUtil.JdfBundleModel

let internal field<'T> name nullable = DataField<'T>(name, Nullable nullable) :> DataField

let internal row values =
    let result = Dictionary<string, obj>()
    values |> Seq.iter (fun (name, value) -> result.Add(name, value))
    result :> IDictionary<string, obj>

let internal nullableObj value =
    value |> Option.map box |> Option.defaultValue null

let internal localDateString (value: LocalDate) =
    value.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture)

let internal nonEmptyText (value: string option) =
    value
    |> Option.bind (fun text ->
        if String.IsNullOrWhiteSpace(text) then None else Some text)

let internal serviceNoteTypeName = function
    | JdfModel.Service -> "service"
    | JdfModel.ServiceAlso -> "service_also"
    | JdfModel.ServiceOnly -> "service_only"
    | JdfModel.NoService -> "no_service"
    | JdfModel.ServiceOddWeeks -> "odd_weeks"
    | JdfModel.ServiceEvenWeeks -> "even_weeks"
    | JdfModel.ServiceOddWeeksFromTo -> "odd_weeks_from_to"
    | JdfModel.ServiceEvenWeeksFromTo -> "even_weeks_from_to"

let internal restrictionLookup (batch: JdfModel.JdfBatch) =
    let result = Dictionary<int, int>()
    for attribute in batch.attributeRefs do
        let bit =
            match attribute.value with
            | JdfModel.TravelExclusion0 -> 1
            | JdfModel.TravelExclusion1 -> 2
            | JdfModel.TravelExclusion2 -> 4
            | JdfModel.TravelExclusion3 -> 8
            | _ -> 0
        if bit <> 0 then result.[attribute.attributeId] <- bit
    result

let internal restrictionMask (lookup: Dictionary<int, int>) (attributes: int option array) =
    let mutable mask = 0
    for attributeId in attributes do
        match attributeId with
        | Some id ->
            match lookup.TryGetValue(id) with
            | true, bit -> mask <- mask ||| bit
            | _ -> ()
        | None -> ()
    mask

let internal restrictionGroupCodes mask = seq {
    if mask &&& 1 <> 0 then yield "§"
    if mask &&& 2 <> 0 then yield "A"
    if mask &&& 4 <> 0 then yield "B"
    if mask &&& 8 <> 0 then yield "C"
}

let internal withOwnerOrdinals ownerKey values =
    values
    |> Seq.groupBy ownerKey
    |> Seq.collect (fun (_, ownedValues) ->
        ownedValues |> Seq.mapi (fun index value -> index + 1, value))

let internal sourceSegment (value: string) = Uri.EscapeDataString(value)

let internal routeNoticeId routeId distinction noticeId =
    $"jdf:notice:route:{sourceSegment routeId}:{distinction}:{noticeId}"

let internal tripNoticeId routeId distinction tripId noticeId =
    $"jdf:notice:trip:{sourceSegment routeId}:{distinction}:{tripId}:{noticeId}"

let internal reservationNoticeId routeId distinction tripId ordinal =
    $"jdf:notice:reservation:{sourceSegment routeId}:{distinction}:{tripId}:{ordinal}"

let internal transferId routeId distinction tripId ordinal =
    $"jdf:transfer:{sourceSegment routeId}:{distinction}:{tripId}:{ordinal}"

let internal restrictionId routeId distinction tripId routeStopId =
    $"jdf:restriction:{sourceSegment routeId}:{distinction}:{tripId}:{routeStopId}"

let internal routeStopSourceId routeId distinction sourceRouteStopId =
    $"jdf:route-stop:{sourceSegment routeId}:{distinction}:{sourceRouteStopId}"

let internal table fields rows =
    { fields = fields; rows = rows |> Array.map (fun value -> value :> IDictionary<string, obj>) }

let internal callIsEmitted (call: JdfModel.TripStop) =
    match call.departureTime with
    | Some JdfModel.Passing | Some JdfModel.NotPassing -> false
    | None when call.arrivalTime = None -> false
    | _ -> true

type internal CallDerivedFacts = {
    emittedCallCount: int64
    tripRestrictionAssignments: struct (string * int64 * string) array
    filteredRestrictionIds: string array
    unjoinableRestrictionIds: string array
    singletonRestrictionDiagnostics: Diagnostic array
    conflictingPostDiagnostics: Diagnostic array
    authoredModes: Dictionary<struct (int64 * string), HashSet<JdfModel.TransportMode>>
}

let internal scanCallDerivedFacts (batch: JdfModel.JdfBatch) (retainedTrips: HashSet<string>)
                                 (candidateStopsWithMultiple: HashSet<int64>)
                                 progress =
    let restrictions = restrictionLookup batch
    let routeRestrictions = Dictionary<struct(string*int*int64),int>()
    for stop in batch.routeStops do
        let mask = restrictionMask restrictions stop.attributes
        routeRestrictions.[struct(stop.routeId,stop.routeDistinction,stop.routeStopId)] <- mask
    let groupCodes = Array.init 16 (restrictionGroupCodes >> Seq.toArray)
    let modeByRoute = Dictionary<struct(string*int),JdfModel.TransportMode>()
    for route in batch.routes do
        modeByRoute.[struct(route.id,route.idDistinction)] <- route.transportMode
    let assignments = ResizeArray<struct(string*int64*string)>()
    let filtered = ResizeArray<string>()
    let unjoinable = ResizeArray<string>()
    let singletonDiagnostics = ResizeArray<Diagnostic>()
    let authoredModes = Dictionary<struct(int64*string),HashSet<JdfModel.TransportMode>>()
    let postNumbers = Dictionary<struct(int64*int64),HashSet<string>>()
    let mutable currentTripId: string = null
    let mutable currentRoute: string = null
    let mutable currentDistinction = 0
    let mutable currentTrip = 0L
    let mutable retained = false
    let mutable emittedCallCount = 0L
    let mutable restrictionMembers = Dictionary<string,HashSet<int64>>(StringComparer.Ordinal)
    let flushTrip () =
        if not (isNull currentTripId) then
            let tripId = currentTripId
            for pair in restrictionMembers do
                if pair.Value.Count = 1 then
                    singletonDiagnostics.Add({
                        severity = "warning"; code = "singleton_travel_restriction"
                        sourceObjectId = $"{tripId}:restriction-group:{sourceSegment pair.Key}"
                        message = "Effective travel-exclusion group has only one emitted call" })
        restrictionMembers <- Dictionary<string,HashSet<int64>>(StringComparer.Ordinal)
    for index=0 to batch.tripStops.Count-1 do
        let call=batch.tripStops.[index]
        if isNull currentTripId || currentRoute <> call.routeId || currentDistinction <> call.routeDistinction || currentTrip <> call.tripId then
            flushTrip()
            currentRoute <- call.routeId
            currentDistinction <- call.routeDistinction
            currentTrip <- call.tripId
            currentTripId <- JdfGtfsRules.jdfTripId call.routeId call.routeDistinction call.tripId
            retained <- retainedTrips.Contains(currentTripId)
        let tripId = currentTripId
        let emitted=callIsEmitted call
        if retained && emitted then emittedCallCount <- emittedCallCount+1L
        let callMask = restrictionMask restrictions call.attributes
        let callRestrictionGroups = groupCodes.[callMask]
        if callRestrictionGroups.Length>0 then
            let sourceId=restrictionId call.routeId call.routeDistinction call.tripId call.routeStopId
            if not retained then filtered.Add(sourceId)
            elif not emitted then unjoinable.Add(sourceId)
            else
                for groupCode in callRestrictionGroups do
                    assignments.Add(struct(tripId,call.routeStopId,groupCode))
        if retained && emitted then
            let routeMask = routeRestrictions.[struct(call.routeId,call.routeDistinction,call.routeStopId)]
            for groupCode in groupCodes.[routeMask ||| callMask] do
                let members =
                    match restrictionMembers.TryGetValue(groupCode) with
                    | true,values -> values
                    | _ ->
                        let values=HashSet<int64>()
                        restrictionMembers.[groupCode] <- values
                        values
                members.Add(call.routeStopId) |> ignore
        if candidateStopsWithMultiple.Contains(call.stopId) then
            let authoredKey =
                match call.stopPostId,nonEmptyText call.stopPostNum with
                | Some value,_ -> Some $"id:{value}"
                | None,Some value -> Some $"num:{value}"
                | _ -> None
            match authoredKey with
            | Some key ->
                let compound=struct(call.stopId,key)
                let modes =
                    match authoredModes.TryGetValue(compound) with
                    | true,values -> values
                    | _ ->
                        let values=HashSet<JdfModel.TransportMode>()
                        authoredModes.[compound] <- values
                        values
                modes.Add(modeByRoute.[struct(call.routeId,call.routeDistinction)]) |> ignore
            | None -> ()
        match call.stopPostId,call.stopPostNum |> Option.bind JdfGtfsRules.nonEmptyTrimmed with
        | Some postId,Some postNumber ->
            let key=struct(call.stopId,postId)
            let numbers =
                match postNumbers.TryGetValue(key) with
                | true,values -> values
                | _ ->
                    let values=HashSet<string>(StringComparer.Ordinal)
                    postNumbers.[key] <- values
                    values
            numbers.Add(postNumber) |> ignore
        | _ -> ()
        if (index+1)%250_000=0 then progress (int64(index+1)) (Some(int64 batch.tripStops.Count))
    flushTrip()
    let conflictingPosts =
        postNumbers
        |> Seq.choose (fun pair ->
            if pair.Value.Count <= 1 then None else
            let struct(stopId,postId) = pair.Key
            let numbers = pair.Value |> Seq.sort |> String.concat ","
            Some { severity = "warning"; code = "conflicting_post_numbers"
                   sourceObjectId = $"jdf:stop:{stopId}:post:id:{postId}"
                   message = $"Authoritative post has conflicting display numbers: {numbers}" })
        |> Seq.toArray
    { emittedCallCount=emittedCallCount
      tripRestrictionAssignments=assignments |> Seq.distinct |> Seq.sort |> Seq.toArray
      filteredRestrictionIds=filtered |> Seq.distinct |> Seq.sort |> Seq.toArray
      unjoinableRestrictionIds=unjoinable |> Seq.distinct |> Seq.sort |> Seq.toArray
      singletonRestrictionDiagnostics=singletonDiagnostics |> Seq.sortBy (fun value -> value.sourceObjectId) |> Seq.toArray
      conflictingPostDiagnostics=conflictingPosts
      authoredModes=authoredModes }

// Parquet is deliberately limited to source facts that cannot be reconstructed
// from standard GTFS plus the Oběhy extension tables. Snapshot identity belongs
// in file metadata and manifest.json rather than being repeated on every row.
let internal transportModeCode = function
    | JdfModel.Bus -> "A"
    | JdfModel.Tram -> "E"
    | JdfModel.Trolleybus -> "T"
    | JdfModel.CableCar -> "L"
    | JdfModel.Metro -> "M"
    | JdfModel.Ferry -> "P"

let internal getTableProducers (sourceTransportModes: Map<string * int, JdfModel.TransportMode>)
                      (batch: JdfModel.JdfBatch) (feed: GtfsModel.GtfsFeed)
                      (postPlan: JdfPostPlan.PostEstimationPlan)
                      (callFacts: CallDerivedFacts)
                      (progress: string -> int64 -> int64 option -> unit)
                      (emittedTransferCalls: HashSet<struct (string * int64)>) =
    let restrictionLookup = restrictionLookup batch
    let stopLocations =
        batch.stopLocations
        |> Seq.groupBy (fun location -> location.stopId)
        |> Seq.map (fun (stopId, locations) ->
            stopId,
            (locations
             |> Seq.sortBy (fun location ->
                 match location.precision with
                 | JdfModel.StopPrecise -> 0
                 | JdfModel.Estimated -> 1)
             |> Seq.head))
        |> Map
    let retainedTripIds = feed.trips |> Seq.map (fun trip -> trip.id) |> Set
    let retainedRouteIds = feed.routes |> Seq.map (fun route -> route.id) |> Set
    let retainedStopIds = feed.stops |> Seq.map (fun stop -> stop.id) |> Set
    let stopLocationSources =
        batch.stopLocationSources |> Seq.map (fun value -> value.stopId, value.source) |> Map

    let routes () =
        batch.routes
        |> Array.sortBy (fun route -> route.id, route.idDistinction)
        |> Array.map (fun route -> row [
            "gtfs_route_id", box (JdfGtfsRules.gtfsRouteId batch route.id route.idDistinction)
            "source_route_id", box (JdfGtfsRules.jdfSourceRouteId route.id route.idDistinction)
            "route_distinction", box route.idDistinction
            "source_agency_id", box route.agencyId
            "source_agency_distinction", box route.agencyDistinction
            "source_transport_mode", box (sourceTransportModes.[route.id, route.idDistinction] |> transportModeCode)
            "effective_transport_mode", box (transportModeCode route.transportMode)
            "valid_from", box (localDateString route.timetableValidFrom)
            "valid_to", box (localDateString route.timetableValidTo)
            "detour", box route.detour ])

    let stopPlaces () =
        batch.stops
        |> Array.filter (fun stop ->
            retainedStopIds.Contains(JdfGtfsRules.jdfStopId stop.id))
        |> Array.sortBy (fun stop -> stop.id)
        |> Array.map (fun stop ->
            let location = stopLocations |> Map.tryFind stop.id
            let coordinatesMissing =
                location
                |> Option.map (fun value -> value.lat = 0m && value.lon = 0m)
                |> Option.defaultValue true
            let coordinatePrecision =
                if coordinatesMissing then "missing"
                else
                    match location.Value.precision with
                    | JdfModel.StopPrecise -> "stop"
                    | JdfModel.Estimated -> "estimated"
            row [
                "gtfs_stop_id", box (JdfGtfsRules.jdfStopId stop.id)
                "town", box stop.town
                "district", nullableObj stop.district
                "nearby_place", nullableObj stop.nearbyPlace
                "country", nullableObj stop.country
                "coordinates_missing", box coordinatesMissing
                "coordinate_precision", box coordinatePrecision
                "coordinate_source", nullableObj (stopLocationSources |> Map.tryFind stop.id) ])

    let locationFeatures () =
        batch.stops
        |> Seq.collect (fun stop ->
            let gtfsStopId = JdfGtfsRules.jdfStopId stop.id
            if not (retainedStopIds.Contains(gtfsStopId)) then Seq.empty else
            Jdf.parseAttributes batch stop.attributes
            |> Seq.map (fun attribute ->
                let code = attribute.CsvSerialize()
                row [
                    "gtfs_stop_id", box gtfsStopId; "source_code", box code
                    "feature_kind", box (match attribute with | JdfModel.WheelchairAccessible -> "wheelchair_boarding_accessible" | _ -> "jdf_stop_attribute")
                    "source_object_id", box $"{gtfsStopId}:attribute:{Uri.EscapeDataString(code)}" ]))
        |> Seq.sortBy (fun value -> string value.["gtfs_stop_id"], string value.["source_code"])
        |> Seq.toArray

    let tripFeatures () : seq<JrUtil.Serving.FeatureWriter.Row> =
        batch.trips
        |> Seq.collect (fun trip ->
            let gtfsTripId = JdfGtfsRules.jdfTripId trip.routeId trip.routeDistinction trip.id
            if not (retainedTripIds.Contains(gtfsTripId)) then Seq.empty
            else
                Jdf.parseAttributes batch trip.attributes
                |> Seq.map (fun attribute ->
                    let code = attribute.CsvSerialize()
                    let kind =
                        match attribute with
                        | JdfModel.WheelchairAccessible -> "wheelchair_accessible_full"
                        | JdfModel.PartlyWheelchairAccessible -> "wheelchair_accessible_partial"
                        | JdfModel.ReservationAvailable -> "reservation_available"
                        | JdfModel.OnlyWithReservation -> "reservation_required"
                        | JdfModel.BicycleTransport -> "bicycle_transport"
                        | _ -> "jdf_trip_attribute"
                    { scope = "trip"; route = ""; trip = gtfsTripId
                      callSequence = Nullable(); service = ""; code = code; kind = kind
                      note = ""
                      sourceObject = $"{gtfsTripId}:attribute:{Uri.EscapeDataString(code)}" }))

    let routeStopZones () =
        batch.routeStops
        |> Seq.collect (fun routeStop ->
            Jdf.normalizeZoneTokens [routeStop.zone]
            |> Seq.mapi (fun index zoneCode ->
                row [
                    "gtfs_route_id", box (JdfGtfsRules.gtfsRouteId batch routeStop.routeId routeStop.routeDistinction)
                    "source_route_version", box routeStop.routeDistinction
                    "source_route_stop_id", box routeStop.routeStopId
                    "zone_id", box (JdfGtfsRules.jdfSourceZoneId routeStop.routeId routeStop.routeDistinction zoneCode)
                    "zone_order", box index ]))
        |> Seq.distinctBy (fun value ->
            value.["gtfs_route_id"], value.["source_route_version"], value.["source_route_stop_id"], value.["zone_id"])
        |> Seq.sortBy (fun value ->
            string value.["gtfs_route_id"], unbox<int> value.["source_route_version"],
            unbox<int64> value.["source_route_stop_id"],
            unbox<int> value.["zone_order"], string value.["zone_id"])
        |> Seq.toArray

    let notices () : seq<JrUtil.Serving.NoteWriter.Row> = seq {
        let date value = value |> Option.map (fun (day: LocalDate) -> Nullable(DateOnly(day.Year, day.Month, day.Day))) |> Option.defaultValue (Nullable())
        let empty id kind route trip : JrUtil.Serving.NoteWriter.Row = {
            id = id; kind = kind; route = route; trip = trip; label = ""; text = ""
            validFrom = Nullable(); validTo = Nullable(); serviceNoteType = "" }
        for notice in batch.routeInfo do
            let route = JdfGtfsRules.gtfsRouteId batch notice.routeId notice.routeDistinction
            if not (String.IsNullOrWhiteSpace(notice.text)) && retainedRouteIds.Contains(route) then
                yield { empty (routeNoticeId notice.routeId notice.routeDistinction notice.id) "route_information" route "" with text = notice.text }
        for notice in batch.serviceNotes do
            let text = nonEmptyText notice.note
            let label = nonEmptyText (Some notice.designation)
            if not (notice.noteType.IsSome && text.IsNone) && (text.IsSome || label.IsSome) then
                let trip = JdfGtfsRules.jdfTripId notice.routeId notice.routeDistinction notice.tripId
                if retainedTripIds.Contains(trip) then
                    yield { empty (tripNoticeId notice.routeId notice.routeDistinction notice.tripId notice.id) "service_note" "" trip with
                                text = text |> Option.defaultValue ""; label = label |> Option.defaultValue ""
                                validFrom = date notice.dateFrom; validTo = date notice.dateTo
                                serviceNoteType = notice.noteType |> Option.map serviceNoteTypeName |> Option.defaultValue "" }
        for ordinal, notice in batch.reservationOptions |> withOwnerOrdinals (fun notice -> notice.routeId, notice.routeDistinction, notice.tripId) do
            let trip = JdfGtfsRules.jdfTripId notice.routeId notice.routeDistinction notice.tripId
            if not (String.IsNullOrWhiteSpace(notice.note)) && retainedTripIds.Contains(trip) then
                yield { empty (reservationNoticeId notice.routeId notice.routeDistinction notice.tripId ordinal) "reservation" "" trip with text = notice.note }
    }
    let transfers () =
        batch.transfers
        |> withOwnerOrdinals (fun transfer -> transfer.routeId, transfer.routeDistinction, transfer.tripId)
        |> Seq.choose (fun (ordinal, transfer) ->
            let gtfsTripId =
                JdfGtfsRules.jdfTripId transfer.routeId transfer.routeDistinction transfer.tripId
            if not (retainedTripIds.Contains gtfsTripId)
               || not (emittedTransferCalls.Contains(struct (gtfsTripId, transfer.routeStopId))) then None
            else Some (row [
                "source_transfer_id", box (transferId transfer.routeId transfer.routeDistinction transfer.tripId ordinal)
                "gtfs_trip_id", box gtfsTripId
                "source_route_stop_id", box transfer.routeStopId
                "transfer_type", box transfer.transferType
                "transfer_route_id", nullableObj transfer.transferRouteId
                "transfer_stop_id", nullableObj transfer.transferStopId
                "transfer_stop_post_id", nullableObj transfer.transferStopPostId
                "transfer_end_stop_id", nullableObj transfer.transferEndStopId
                "transfer_end_stop_post_id", nullableObj transfer.transferEndStopPostId
                "wait_minutes", nullableObj transfer.waitMinutes
                "note", nonEmptyText transfer.note |> nullableObj ]))
        |> Seq.sortBy (fun value ->
            string value.["gtfs_trip_id"], unbox<int64> value.["source_route_stop_id"],
            string value.["source_transfer_id"])
        |> Seq.toArray

    let restrictions () =
        let routeStopAssignments =
            batch.routeStops
            |> Seq.collect (fun routeStop ->
                let gtfsRouteId =
                    JdfGtfsRules.gtfsRouteId batch routeStop.routeId routeStop.routeDistinction
                if not (retainedRouteIds.Contains gtfsRouteId) then Seq.empty else
                restrictionGroupCodes (restrictionMask restrictionLookup routeStop.attributes)
                |> Seq.map (fun groupCode -> row [
                    "assignment_scope", box "route_stop"
                    "gtfs_route_id", box gtfsRouteId
                    "gtfs_trip_id", null
                    "source_route_version", box routeStop.routeDistinction
                    "source_route_stop_id", box routeStop.routeStopId
                    "group_code", box groupCode ]))
        let tripCallAssignments =
            callFacts.tripRestrictionAssignments
            |> Seq.map (fun struct(gtfsTripId,routeStopId,groupCode) -> row [
                "assignment_scope", box "trip_call"
                "gtfs_route_id", null
                "gtfs_trip_id", box gtfsTripId
                "source_route_version", null
                "source_route_stop_id", box routeStopId
                "group_code", box groupCode ])
        Seq.append routeStopAssignments tripCallAssignments
        |> Seq.distinctBy (fun value ->
            value.["assignment_scope"], value.["gtfs_route_id"], value.["gtfs_trip_id"],
            value.["source_route_version"], value.["source_route_stop_id"], value.["group_code"])
        |> Seq.sortBy (fun value ->
            string value.["assignment_scope"], string value.["gtfs_route_id"],
            string value.["gtfs_trip_id"], string value.["source_route_version"],
            unbox<int64> value.["source_route_stop_id"], string value.["group_code"])
        |> Seq.toArray

    let candidateLookup =
        postPlan.physicalHypotheses
        |> Seq.groupBy (fun candidate -> candidate.stopId)
        |> Seq.map (fun (stopId, values) -> stopId,values |> Seq.toArray)
        |> Map
    let derivedPostLocations () =
        postPlan.locations
        |> Array.map (fun location ->
            let candidates = candidateLookup |> Map.tryFind location.stopId |> Option.defaultValue [||]
            let provenance =
                candidates
                |> Seq.filter (fun candidate -> location.candidateIds |> Array.contains candidate.hypothesisId)
                |> Seq.collect _.sources
                |> Seq.distinct
                |> Seq.sort
                |> String.concat ";"
            let representative = location.representativeCandidateId
            let diagnosticLabel =
                representative
                |> Option.bind (fun candidateId ->
                    candidates |> Array.sortBy (fun value -> value.hypothesisId)
                    |> Array.tryFindIndex (fun value -> value.hypothesisId = candidateId))
                |> Option.map (fun index -> $"O{index + 1}")
            row [
                "derived_location_id", box location.locationId
                "gtfs_stop_place_id", box (JdfGtfsRules.jdfStopId location.stopId)
                "selection_kind", box location.selectionKind
                "physical_candidate_id", if location.selectionKind = "physical" then representative |> nullableObj else null
                "representative_candidate_id", representative |> nullableObj
                "diagnostic_label", diagnosticLabel |> nullableObj
                "latitude", box (float location.lat)
                "longitude", box (float location.lon)
                "candidate_ids", box (String.Join(";", location.candidateIds))
                "provenance", box provenance ])

    let authoredTargetId stopId (key: string) =
        if key.StartsWith("id:", StringComparison.Ordinal) then
            JdfGtfsRules.jdfStopPostId stopId (Int64.Parse(key.Substring(3), CultureInfo.InvariantCulture))
        else JdfGtfsRules.jdfStopPostNumId stopId (key.Substring(4))
    let rejectedCandidateIds stopId (selection: JdfPostPlan.DerivedPostSelection) =
        candidateLookup
        |> Map.tryFind stopId
        |> Option.defaultValue [||]
        |> Array.map (fun candidate -> candidate.hypothesisId)
        |> Array.filter (fun candidateId -> not (selection.candidateIds |> Array.contains candidateId))
        |> Array.sort
        |> fun ids -> String.Join(";", ids)
    let contextStopIds selector (contexts: JdfPostPlan.DerivedPostContext array) =
        contexts
        |> Seq.choose selector
        |> Seq.distinct
        |> Seq.sort
        |> Seq.map (JdfGtfsRules.jdfStopId )
        |> String.concat ";"
        |> function value when String.IsNullOrEmpty(value) -> None | value -> Some value
    let contextRoles (contexts: JdfPostPlan.DerivedPostContext array) =
        contexts
        |> Seq.map (fun context -> context.sameStopBlockRole)
        |> Seq.distinct |> Seq.sort |> String.concat ";"
    let movementFamilyId key =
        match postPlan.movementFamilyIds.TryGetValue(key) with
        | true,value -> Some value
        | _ -> None
    let modeByRoute = Dictionary<struct (string * int), string>()
    for route in batch.routes do
        modeByRoute.[struct (route.id, route.idDistinction)] <-
            transportModeCode route.transportMode
    let candidateStopsWithMultiple =
        candidateLookup
        |> Seq.choose (fun pair ->
            if pair.Value.Length >= 2 then Some pair.Key else None)
        |> HashSet
    let authoredKeys = HashSet<struct (int64 * string)>()
    for post in batch.stopPosts do
        if candidateStopsWithMultiple.Contains(post.stopId) then
            authoredKeys.Add(struct (post.stopId, $"id:{post.stopPostId}")) |> ignore
    let authoredModes = callFacts.authoredModes
    for compoundKey in authoredModes.Keys do authoredKeys.Add(compoundKey) |> ignore
    let modesFor stopId key =
        match authoredModes.TryGetValue(struct (stopId, key)) with
        | true, modes -> modes |> Seq.map transportModeCode |> Seq.sort |> String.concat ";"
        | _ -> ""
    let authoredAssignments () =
        postPlan.authored
        |> Seq.sortBy (fun pair -> pair.Key)
        |> Seq.map (fun pair ->
            let stopId, key = pair.Key
            let selection = pair.Value
            let contexts = postPlan.authoredContexts |> Map.tryFind (stopId, key) |> Option.defaultValue [||]
            let modes = modesFor stopId key
            { targetGtfsStopId=authoredTargetId stopId key; assignmentKind="authored"
              derivedLocationId=Some selection.locationId; mode=modes
              lineId=None; direction=None; patternHash=None; patternPosition=None
              movementFamilyId=None
              contextPreviousStopId=contextStopIds (fun context -> context.previousStopId) contexts
              contextNextStopId=contextStopIds (fun context -> context.nextStopId) contexts
              sameStopBlockRole=contextRoles contexts; score=Some selection.score; margin=selection.margin
              selectedCandidates=String.Join(";", selection.candidateIds)
              rejectedCandidates=rejectedCandidateIds stopId selection; status="positioned" })
    let authoredInsufficientAssignments () =
        authoredKeys
        |> Seq.sort
        |> Seq.choose (fun struct (stopId, key) ->
            if postPlan.authored.ContainsKey((stopId, key)) then None else
            let modes = modesFor stopId key
            let rejected =
                candidateLookup.[stopId]
                |> Array.map (fun candidate -> candidate.hypothesisId)
                |> Array.sort |> fun ids -> String.Join(";", ids)
            Some { targetGtfsStopId=authoredTargetId stopId key; assignmentKind="authored"
                   derivedLocationId=None; mode=modes; lineId=None; direction=None
                   patternHash=None; patternPosition=None; contextPreviousStopId=None
                   movementFamilyId=None
                   contextNextStopId=None; sameStopBlockRole="unresolved"; score=None; margin=None
                   selectedCandidates=""; rejectedCandidates=rejected
                   status="insufficient_parent_centroid" })
    let assignmentTotal =
        int64 (postPlan.authored.Count + authoredKeys.Count - postPlan.authored.Count
               + postPlan.calls.Count + postPlan.unresolvedPatternContexts.Length)
    let mutable assignmentProgress = 0L
    let reportAssignmentProgress () =
        assignmentProgress <- assignmentProgress + 1L
        if assignmentProgress % 100_000L = 0L then
            progress "prepare-derived-post-assignments" assignmentProgress (Some assignmentTotal)
    let internalAssignments () =
        postPlan.calls
        |> Seq.sortBy (fun pair ->
            let key=pair.Key
            key.stopId,key.mode,key.lineId,key.routeDistinction,key.direction,
            key.patternHash,key.position,key.sameStopBlockRole)
        |> Seq.map (fun pair ->
            reportAssignmentProgress ()
            let stopId, mode = pair.Key.stopId, pair.Key.mode
            let selection = pair.Value
            let context = postPlan.callContexts.[pair.Key]
            { targetGtfsStopId = JdfToGtfs.inferredPostId postPlan selection
              assignmentKind="internal"; derivedLocationId=Some selection.locationId
              mode=transportModeCode mode; lineId=Some pair.Key.lineId
              direction=Some pair.Key.direction; patternHash=Some pair.Key.patternHash
              patternPosition=Some pair.Key.position
              movementFamilyId=movementFamilyId pair.Key
              contextPreviousStopId=context.previousStopId |> Option.map (JdfGtfsRules.jdfStopId )
              contextNextStopId=context.nextStopId |> Option.map (JdfGtfsRules.jdfStopId )
              sameStopBlockRole=context.sameStopBlockRole; score=Some selection.score; margin=selection.margin
              selectedCandidates=String.Join(";", selection.candidateIds)
              rejectedCandidates=rejectedCandidateIds stopId selection; status="positioned" })
    let internalCentroidAssignments () =
        postPlan.unresolvedPatternContexts
        |> Seq.map (fun contextKey ->
            reportAssignmentProgress ()
            let context = postPlan.callContexts.[contextKey]
            let rejected =
                candidateLookup |> Map.tryFind contextKey.stopId |> Option.defaultValue [||]
                |> Array.map (fun value -> value.hypothesisId) |> Array.sort
                |> fun values -> String.Join(";",values)
            { targetGtfsStopId = $"{JdfGtfsRules.jdfStopId contextKey.stopId}:unspecified"
              assignmentKind="internal"; derivedLocationId=None; mode=transportModeCode contextKey.mode
              lineId=Some contextKey.lineId; direction=Some contextKey.direction
              patternHash=Some contextKey.patternHash; patternPosition=Some contextKey.position
              movementFamilyId=movementFamilyId contextKey
              contextPreviousStopId=context.previousStopId |> Option.map (JdfGtfsRules.jdfStopId )
              contextNextStopId=context.nextStopId |> Option.map (JdfGtfsRules.jdfStopId )
              sameStopBlockRole=contextKey.sameStopBlockRole; score=None; margin=None
              selectedCandidates=""; rejectedCandidates=rejected; status="centroid_fallback" })
    let derivedPostAssignments () : seq<DerivedPostAssignmentRow> =
        assignmentProgress <- 0L
        let authored =
            Seq.append (authoredAssignments ()) (authoredInsufficientAssignments ())
            |> Seq.map (fun value ->
                reportAssignmentProgress ()
                value)
        Seq.concat [ authored; internalAssignments (); internalCentroidAssignments () ]
    let hypothesisByCandidate =
        postPlan.physicalHypotheses
        |> Seq.collect (fun hypothesis ->
            hypothesis.memberCandidateIds |> Seq.map (fun candidateId -> candidateId,hypothesis.hypothesisId))
        |> Map
    let candidateEvidenceRows () =
        batch.postCandidateEvidence
        |> Array.sortBy (fun value -> value.stopId, value.candidateId, value.observationId)
        |> Array.map (fun evidence ->
            let hypothesisId=hypothesisByCandidate |> Map.tryFind evidence.candidateId
            row [
                "gtfs_stop_place_id", box (JdfGtfsRules.jdfStopId evidence.stopId)
                "candidate_id", box evidence.candidateId; "observation_id", box evidence.observationId
                "hypothesis_id", hypothesisId |> nullableObj
                "source_kind", box evidence.sourceKind; "source_object_id", evidence.sourceObjectId |> nullableObj
                "observed_at", evidence.observedAt |> nullableObj
                "latitude", box (float evidence.lat); "longitude", box (float evidence.lon)
                "support_weight", box (float evidence.supportWeight); "raw_tags", box evidence.rawTags
                "explicit_modes", box evidence.explicitModes; "denied_modes", box evidence.deniedModes
                "lifecycle", box evidence.lifecycle ])
    let physicalHypothesisRows () =
        postPlan.physicalHypotheses
        |> Array.map (fun hypothesis -> row [
            "gtfs_stop_place_id",box(JdfGtfsRules.jdfStopId hypothesis.stopId)
            "hypothesis_id",box hypothesis.hypothesisId
            "member_observation_ids",box(String.Join(";",hypothesis.memberObservationIds))
            "member_legacy_candidate_ids",box(String.Join(";",hypothesis.memberCandidateIds))
            "representative_candidate_id",box hypothesis.representativeCandidateId
            "latitude",box(float hypothesis.lat); "longitude",box(float hypothesis.lon)
            "sources",box(String.Join(";",hypothesis.sources)) ])
    let stringField name nullable = field<string> name nullable
    let intField name nullable = field<int> name nullable
    let int64Field name nullable = field<int64> name nullable
    let boolField name nullable = field<bool> name nullable
    let doubleField name nullable = field<double> name nullable
    let producer fields rows = fun () -> table fields (rows ())
    [|
        "source_route_metadata.parquet", producer [|
            stringField "gtfs_route_id" false; stringField "source_route_id" false
            intField "route_distinction" false; stringField "source_agency_id" false
            intField "source_agency_distinction" false; stringField "source_transport_mode" false
            stringField "effective_transport_mode" false; stringField "valid_from" false
            stringField "valid_to" false; boolField "detour" false |] routes
        "source_stop_metadata.parquet", producer [|
            stringField "gtfs_stop_id" false; stringField "town" false
            stringField "district" true; stringField "nearby_place" true
            stringField "country" true; boolField "coordinates_missing" false
            stringField "coordinate_precision" false; stringField "coordinate_source" true |] stopPlaces
        "source_location_feature_metadata.parquet", producer [|
            stringField "gtfs_stop_id" false; stringField "source_code" false
            stringField "feature_kind" false; stringField "source_object_id" false |] locationFeatures
        "source_route_stop_zone_metadata.parquet", producer [|
            stringField "gtfs_route_id" false; intField "source_route_version" false
            int64Field "source_route_stop_id" false
            stringField "zone_id" false; intField "zone_order" false |] routeStopZones
        "source_transfer_metadata.parquet", producer [|
            stringField "source_transfer_id" false; stringField "gtfs_trip_id" false
            int64Field "source_route_stop_id" false; stringField "transfer_type" false
            int64Field "transfer_route_id" true; int64Field "transfer_stop_id" true
            int64Field "transfer_stop_post_id" true; int64Field "transfer_end_stop_id" true
            int64Field "transfer_end_stop_post_id" true; intField "wait_minutes" true
            stringField "note" true |] transfers
        "source_travel_restriction_metadata.parquet", producer [|
            stringField "assignment_scope" false; stringField "gtfs_route_id" true
            stringField "gtfs_trip_id" true; intField "source_route_version" true
            int64Field "source_route_stop_id" false
            stringField "group_code" false |] restrictions
        "derived_post_locations.parquet", producer [|
            stringField "derived_location_id" false; stringField "gtfs_stop_place_id" false
            stringField "selection_kind" false; stringField "physical_candidate_id" true
            stringField "representative_candidate_id" true
            stringField "diagnostic_label" true; doubleField "latitude" false
            doubleField "longitude" false; stringField "candidate_ids" false
            stringField "provenance" false |] derivedPostLocations
        "post_candidate_evidence.parquet", producer [|
            stringField "gtfs_stop_place_id" false; stringField "candidate_id" false
            stringField "observation_id" false; stringField "source_kind" false
            stringField "hypothesis_id" true
            stringField "source_object_id" true; stringField "observed_at" true
            doubleField "latitude" false; doubleField "longitude" false
            doubleField "support_weight" false; stringField "raw_tags" false
            stringField "explicit_modes" false; stringField "denied_modes" false
            stringField "lifecycle" false |] candidateEvidenceRows
        "post_physical_hypotheses.parquet", producer [|
            stringField "gtfs_stop_place_id" false; stringField "hypothesis_id" false
            stringField "member_observation_ids" false; stringField "member_legacy_candidate_ids" false
            stringField "representative_candidate_id" false; doubleField "latitude" false
            doubleField "longitude" false; stringField "sources" false |] physicalHypothesisRows
    |], assignmentTotal, derivedPostAssignments, notices, tripFeatures

let internal writeParquet descriptor path table =
    task {
        let schema = ParquetSchema(table.fields |> Array.map (fun field -> field :> Field))
        let options = ParquetOptions(CompressionMethod = CompressionMethod.Snappy,
                                     RowGroupSize = Nullable 65536)
        let metadata = Dictionary<string, string>()
        metadata.Add("obehy.bundle_version", string BundleVersion)
        metadata.Add("obehy.schema_version", string ParquetSchemaVersion)
        metadata.Add("obehy.source_id", descriptor.sourceId)
        metadata.Add("obehy.snapshot_id", $"sha256:{descriptor.payloadSha256}")
        use stream = File.Open(path, FileMode.CreateNew, FileAccess.Write, FileShare.None)
        do! ParquetSerializer.SerializeUntypedAsync(table.rows, schema, stream, options,
                                                    metadata, CancellationToken.None)
    } |> fun operation -> operation.GetAwaiter().GetResult()
