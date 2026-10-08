// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

module JrUtil.CzPttBundle

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open FSharp.Data
open OsmSharp
open OsmSharp.Streams
open Parquet
open Parquet.Schema
open Parquet.Serialization

open JrUtil
open JrUtil.CzPttCoordinates

let mutable private activeSpillBytes = 0L
let currentSpillBytes () = activeSpillBytes

type StoragePolicy =
    | MemoryBacked
    | SpillBacked

[<Literal>]
let ParquetSchemaVersion = 1

type private Table = {
    fields: DataField array
    rows: IReadOnlyCollection<IDictionary<string, obj>>
}

/// Supplies Parquet.NET with a count-known replayable relation without
/// retaining a second array of row dictionaries. The source relations are
/// immutable for the lifetime of bundle output and may be enumerated twice.
type private ReplayableRows<'a>(rows: seq<'a>) =
    let count = lazy (rows |> Seq.length)
    interface IReadOnlyCollection<'a> with
        member _.Count = count.Value
    interface IEnumerable<'a> with
        member _.GetEnumerator() = rows.GetEnumerator()
    interface System.Collections.IEnumerable with
        member _.GetEnumerator() = rows.GetEnumerator() :> System.Collections.IEnumerator

let private field<'T> name nullable =
    DataField<'T>(name, Nullable nullable) :> DataField

let private row values =
    let result = Dictionary<string, obj>()
    for name, value in values do result.Add(name, value)
    result :> IDictionary<string, obj>

let private nullableObj value =
    value |> Option.map box |> Option.defaultValue null

let private table fields rows = {
    fields = fields
    rows = ReplayableRows<IDictionary<string, obj>>(rows)
}

let private writeParquet (path: string) (value: Table) =
    task {
        let schema = ParquetSchema(value.fields |> Array.map (fun item -> item :> Field))
        let options =
            ParquetOptions(
                CompressionMethod = CompressionMethod.Snappy,
                RowGroupSize = Nullable 65536)
        let metadata = Dictionary<string, string>()
        metadata.Add("obehy.schema_version", string ParquetSchemaVersion)
        metadata.Add("obehy.source_format", "czptt")
        metadata.Add("obehy.bundle_format", "czptt-v1")
        metadata.Add("obehy.table", Path.GetFileNameWithoutExtension(path))
        metadata.Add("obehy.row_count", string value.rows.Count)
        use stream = File.Open(path, FileMode.Create, FileAccess.Write, FileShare.None)
        do! ParquetSerializer.SerializeUntypedAsync(
                value.rows, schema, stream, options, metadata, CancellationToken.None)
    }
    |> fun operation -> operation.GetAwaiter().GetResult()

let private parameterPairs (parameters: CzPttXml.NetworkSpecificParameter array) =
    parameters
    |> Option.ofObj
    |> Option.defaultValue [||]
    |> Seq.collect (fun parameter ->
        let names = parameter.Name |> Option.ofObj |> Option.defaultValue [||]
        let values = parameter.Value |> Option.ofObj |> Option.defaultValue [||]
        Seq.zip (names |> Seq.truncate values.Length)
                (values |> Seq.truncate names.Length))

let private paId (message: CzPttXml.CzpttcisMessage) =
    CzPtt.timetableIdentifier message CzPttXml.ObjectType.Pa
    |> CzPtt.identifierStr

let private idsRole (catalog: CzPttModel.CatalogSnapshot) code =
    catalog.ids
    |> Array.tryFind (fun record -> record.code = code)
    |> Option.map (fun record ->
        let fareZone =
            record.note
            |> Option.exists (fun note ->
                Regex.IsMatch(
                    note, @"(?i)\bpásmo\s+.+\s*$",
                    RegexOptions.CultureInvariant))
        if fareZone then "fare_zone" else "coverage")

let private idsSystem (catalog: CzPttModel.CatalogSnapshot) code =
    catalog.ids
    |> Array.tryFind (fun record -> record.code = code)
    |> Option.map (fun record ->
        let parts = record.abbreviation.Split('_')
        if parts.Length > 1 then parts.[0] else record.abbreviation)


/// Convert CZPTT messages into a feed plus operational and source-metadata
/// sidecars; `outputDirectory` is private scratch for the sidecar spools.
let convert storagePolicy catalog options inputPath outputDirectory
            sr70Path osmPath
            (progress: string -> string -> unit) =
    Directory.CreateDirectory(outputDirectory) |> ignore
    progress "parse-input" "started"
    let outputFull = Path.GetFullPath(outputDirectory)
    let spillPath =
        Path.Combine(
            Path.GetDirectoryName(outputFull),
            $".{Path.GetFileName(outputFull)}.czptt.{Guid.NewGuid():N}.tmp")
    use merger =
        match storagePolicy with
        | MemoryBacked -> new CzPttMerge.CzPttMerger()
        | SpillBacked -> new CzPttMerge.CzPttMerger(spillPath)
    progress "merge-messages" "started"
    CzPtt.parseAll inputPath |> merger.ProcessAll
    activeSpillBytes <- merger.SpillBytes
    progress "parse-input" "completed"
    progress "merge-messages" "completed"
    progress "prepare-identities" "started"
    let sourcePaIds = merger.SourcePaIds |> Set
    let messages =
        merger.SurvivingMessages |> Seq.sortBy paId |> Seq.toArray
    let survivingPaIds = messages |> Seq.map paId |> Set
    let cancelledPaIds =
        Set.difference sourcePaIds survivingPaIds |> Set.toArray |> Array.sort
    progress "prepare-identities" "completed"
    progress "load-coordinate-sources" "started"
    let sr70 = loadSr70Coordinates sr70Path
    let osmCandidates = loadOsmCandidates osmPath
    progress "load-coordinate-sources" "completed"
    progress "convert-gtfs" "started"
    let rawResult =
        CzPttToGtfs.convert
            catalog options sr70.names messages
    progress "convert-gtfs" "completed"
    progress "index-stop-times" "started"
    let basePointIdentityByStopId =
        rawResult.operationalCalls
        |> Seq.filter (fun call -> call.generatedTripIds.Length > 0)
        |> Seq.collect (fun call ->
            seq {
                call.generatedStationId, (call.countryCode, call.primaryCode)
                call.generatedStopId, (call.countryCode, call.primaryCode)
            })
        |> Seq.distinct
        |> Map
    let pointIdentityByStopId =
        rawResult.feed.stops
        |> Seq.choose (fun stop ->
            if stop.id.EndsWith(":platform:BUS", StringComparison.Ordinal) then
                stop.parentStation
                |> Option.bind (fun station ->
                    basePointIdentityByStopId
                    |> Map.tryFind station
                    |> Option.map (fun identity -> stop.id, identity))
            else None)
        |> Seq.append (Map.toSeq basePointIdentityByStopId)
        |> Map
    let usedPointIdentities =
        pointIdentityByStopId |> Map.values |> Seq.distinct |> Seq.sort |> Seq.toArray
    let identityNames =
        rawResult.operationalCalls
        |> Seq.groupBy (fun call -> call.countryCode, call.primaryCode)
        |> Seq.map (fun (identity, calls) ->
            identity,
            calls
            |> Seq.map (fun call -> normalizedName call.name)
            |> Seq.filter (String.IsNullOrWhiteSpace >> not)
            |> Set)
        |> Map
    let ambiguous = HashSet<string>()
    let corridorRejected = HashSet<string>()
    let conflicts =
        ResizeArray<CzPttModel.CoordinateConflictDiagnostic>()
    let osmByPlcIndex =
        osmCandidates
        |> Seq.choose (fun candidate ->
            candidate.plc |> Option.map (fun plc -> plc, candidate))
        |> Seq.groupBy fst
        |> Seq.map (fun (plc, values) ->
            plc, values |> Seq.map snd |> Seq.toArray)
        |> Map
    let osmByName =
        osmCandidates
        |> Seq.collect (fun candidate ->
            candidate.names
            |> Seq.distinct
            |> Seq.map (fun name -> normalizedName name, candidate))
        |> Seq.groupBy fst
        |> Seq.map (fun (name, values) ->
            name, values |> Seq.map snd |> Seq.toArray)
        |> Map
    let osmByCoreName =
        osmCandidates
        |> Seq.collect (fun candidate ->
            candidate.names
            |> Seq.map railwayNameCore
            |> Seq.filter (String.IsNullOrWhiteSpace >> not)
            |> Seq.distinct
            |> Seq.map (fun name -> name, candidate))
        |> Seq.groupBy fst
        |> Seq.map (fun (name, values) ->
            name, values |> Seq.map snd |> Seq.toArray)
        |> Map
    let osmByPlc (country: string, code: string) =
        let expected = (country + code).ToUpperInvariant()
        Map.tryFind expected osmByPlcIndex |> Option.defaultValue [||]
    let journeys: CzPttModel.OperationalCall array array =
        rawResult.operationalCalls
        |> Seq.groupBy (fun call -> call.paId)
        |> Seq.map (fun (_, calls) ->
            calls |> Seq.sortBy (fun call -> call.sourceSequence) |> Seq.toArray)
        |> Seq.toArray
    let occurrencesByIdentity:
            Map<string * string, (CzPttModel.OperationalCall array * int) array> =
        journeys
        |> Seq.collect (fun calls ->
            calls
            |> Seq.mapi (fun index call ->
                (call.countryCode, call.primaryCode), (calls, index)))
        |> Seq.groupBy fst
        |> Seq.map (fun (identity, values) ->
            identity, values |> Seq.map snd |> Seq.toArray)
        |> Map
    let indexedNameCandidates
            (index: Map<string, OsmCandidate array>)
            (transform: string -> string)
            (identity: string * string): OsmCandidate array =
        let expectedNames = Map.tryFind identity identityNames |> Option.defaultValue Set.empty
        expectedNames
        |> Seq.map transform
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> Seq.distinct
        |> Seq.collect (fun name ->
            Map.tryFind name index
            |> Option.defaultValue [||])
        |> Seq.distinctBy (fun (candidate: OsmCandidate) -> candidate.objectId)
        |> Seq.toArray
    let exactNameCandidates (identity: string * string): OsmCandidate array =
        indexedNameCandidates osmByName id identity
    let coreNameCandidates (identity: string * string): OsmCandidate array =
        indexedNameCandidates osmByCoreName railwayNameCore identity
    let fuzzyNameCache =
        Dictionary<string * string, OsmCandidate array>()
    let preparedOsmNames =
        osmCandidates |> Array.map (fun candidate -> candidate.names |> Array.map prepareName)
    let fuzzyNameTime = Diagnostics.Stopwatch()
    let fuzzyNameCandidates (identity: string * string) =
        match fuzzyNameCache.TryGetValue(identity) with
        | true, values -> values
        | _ ->
            fuzzyNameTime.Start()
            let expectedNames =
                Map.tryFind identity identityNames
                |> Option.defaultValue Set.empty
                |> Set.toArray
                |> Array.map (fun name ->
                    let prepared = prepareName name
                    prepared, requiredQualifierTokens prepared)
            let minimumScore = 0.58
            let values =
                Array.zip osmCandidates preparedOsmNames
                |> Array.Parallel.choose (fun (candidate, candidateNames) ->
                    let mutable score = None
                    for candidateName in candidateNames do
                        for expectedName, required in expectedNames do
                            if preparedQualifierCompatible required candidateName then
                                match preparedNameSimilarityAtLeast minimumScore expectedName candidateName with
                                | Some value when score |> Option.forall (fun best -> value > best) ->
                                    score <- Some value
                                | _ -> ()
                    score |> Option.map (fun score -> score, candidate))
                |> Array.sortBy (fun (score, candidate) ->
                    -score, candidate.objectId)
                |> Array.truncate 32
                |> Array.map snd
            fuzzyNameCache.Add(identity, values)
            fuzzyNameTime.Stop()
            values
    let candidateGroups (identity: string * string) =
        [|
            "ref_eu_plc", fun () -> osmByPlc identity
            "normalized_exact_name", fun () -> exactNameCandidates identity
            "normalized_railway_name", fun () -> coreNameCandidates identity
            "normalized_fuzzy_name", fun () -> fuzzyNameCandidates identity
        |]
    let departureTime (call: CzPttModel.OperationalCall) =
        call.departureSeconds |> Option.orElse call.arrivalSeconds
    let arrivalTime (call: CzPttModel.OperationalCall) =
        call.arrivalSeconds |> Option.orElse call.departureSeconds
    let edgePlausibility
            (leftCoordinate: double * double)
            (leftCall: CzPttModel.OperationalCall)
            (rightCoordinate: double * double)
            (rightCall: CzPttModel.OperationalCall) =
        match departureTime leftCall, arrivalTime rightCall with
        | Some left, Some right when right > left ->
            let maximumMeters =
                2000. + float (right - left) / 3600. * 150000.
            Some (distanceMeters leftCoordinate rightCoordinate <= maximumMeters)
        | _ -> None
    let candidatePlausible
            (selected: Map<string * string, SelectedCoordinate>)
            (identity: string * string)
            (candidate: OsmCandidate) =
        let coordinate = candidate.latitude, candidate.longitude
        occurrencesByIdentity
        |> Map.tryFind identity
        |> Option.defaultValue [||]
        |> Array.choose (fun (calls, index) ->
            let edges =
                [|
                    if index > 0 then
                        let previous = calls.[index - 1]
                        match Map.tryFind
                                  (previous.countryCode, previous.primaryCode)
                                  selected with
                        | Some value ->
                            yield!
                                edgePlausibility
                                    (value.latitude, value.longitude)
                                    previous coordinate calls.[index]
                                |> Option.toList
                        | None -> ()
                    if index + 1 < calls.Length then
                        let next = calls.[index + 1]
                        match Map.tryFind
                                  (next.countryCode, next.primaryCode)
                                  selected with
                        | Some value ->
                            yield!
                                edgePlausibility coordinate calls.[index]
                                    (value.latitude, value.longitude) next
                                |> Option.toList
                        | None -> ()
                |]
            if edges.Length = 0 then None
            else Some (Array.forall id edges))
        |> fun evidence ->
            evidence.Length = 0 || Array.exists id evidence
    let candidateCorridorFit
            (selected: Map<string * string, SelectedCoordinate>)
            (identity: string * string)
            (candidate: OsmCandidate) =
        let coordinate = candidate.latitude, candidate.longitude
        occurrencesByIdentity
        |> Map.tryFind identity
        |> Option.defaultValue [||]
        |> Array.fold (fun (count, total) (calls, index) ->
            let neighbors =
                [|
                    if index > 0 then yield calls.[index - 1]
                    if index + 1 < calls.Length then yield calls.[index + 1]
                |]
            neighbors
            |> Array.fold (fun (innerCount, innerTotal) call ->
                match Map.tryFind (call.countryCode, call.primaryCode) selected with
                | None -> innerCount, innerTotal
                | Some value ->
                    innerCount + 1,
                    innerTotal
                    + distanceMeters
                        coordinate
                        (value.latitude, value.longitude))
                (count, total))
            (0, 0.)
    let asSelected
            (methodName: string)
            (candidate: OsmCandidate): SelectedCoordinate = {
        latitude = candidate.latitude
        longitude = candidate.longitude
        source = "osm"
        objectId = Some candidate.objectId
        matchMethod = methodName
    }
    let trySelectCandidate
            (selected: Map<string * string, SelectedCoordinate>)
            (identity: string * string) =
        let country, code = identity
        candidateGroups identity
        |> Array.tryPick (fun (methodName, getCandidates) ->
            let plausible, rejected =
                getCandidates ()
                |> Array.distinctBy (fun candidate -> candidate.objectId)
                |> Array.partition (candidatePlausible selected identity)
            for candidate in rejected do
                corridorRejected.Add(
                    $"{country}:{code}:{methodName}:{candidate.objectId}")
                |> ignore
            match plausible with
            | [| candidate |] -> Some (asSelected methodName candidate)
            | values when values.Length > 1 ->
                ambiguous.Add(
                    $"{country}:{code}:{methodName}:" +
                    (values |> Array.map (fun value -> value.objectId)
                            |> String.concat ","))
                |> ignore
                let ranked =
                    values
                    |> Array.sortBy (fun candidate ->
                        let evidence, distance =
                            candidateCorridorFit selected identity candidate
                        let countryHint =
                            match candidate.countryCode with
                            | Some value when value = country -> 0
                            | None -> 1
                            | Some _ -> 2
                        -evidence, countryHint, distance, candidate.objectId)
                Some (asSelected methodName ranked.[0])
            | _ -> None)
    let authoritativeCoordinates: Map<string * string, SelectedCoordinate> =
        usedPointIdentities
        |> Seq.choose (fun identity ->
            let country, code = identity
            match Map.tryFind identity sr70.coordinates with
            | Some (latitude, longitude) ->
                for candidate in osmByPlc identity do
                    let distance =
                        distanceMeters (latitude, longitude)
                            (candidate.latitude, candidate.longitude)
                    if distance > 1. then
                        conflicts.Add {
                            sourceLocationId = $"{country}:{code}"
                            retainedSource = "sz_sr70"
                            candidateObjectId = candidate.objectId
                            distanceMeters = distance
                        }
                Some (
                    identity,
                    {
                        latitude = latitude
                        longitude = longitude
                        source = "sz_sr70"
                        objectId = Some $"sz-sr70:{code}"
                        matchMethod = "country_primary_code"
                    })
            | None -> None)
        |> Map
    let resolutionOrder =
        journeys
        |> Seq.collect id
        |> Seq.map (fun call -> call.countryCode, call.primaryCode)
        |> Seq.distinct
        |> Seq.toArray
    progress "resolve-coordinates" "started"
    let mutable selectedCoordinates:
            Map<string * string, SelectedCoordinate> = authoritativeCoordinates
    for identity in resolutionOrder do
        if not (Map.containsKey identity selectedCoordinates) then
            match trySelectCandidate selectedCoordinates identity with
            | Some coordinate ->
                selectedCoordinates <- Map.add identity coordinate selectedCoordinates
            | None -> ()
    // Re-evaluate with neighboring OSM choices now available. A rejected primary
    // candidate may be replaced by an alias or exact-name candidate.
    for _ = 1 to 2 do
        for identity in resolutionOrder do
            if not (Map.containsKey identity authoritativeCoordinates) then
                let withoutCurrent = Map.remove identity selectedCoordinates
                match trySelectCandidate withoutCurrent identity with
                | Some coordinate ->
                    selectedCoordinates <- Map.add identity coordinate withoutCurrent
                | None ->
                    selectedCoordinates <- withoutCurrent
    progress "resolve-coordinates" "completed"
    Serilog.Log.Information(
        "CZPTT fuzzy OSM name matching: {Points} points against {Candidates} candidates in {ElapsedMs} ms",
        fuzzyNameCache.Count, osmCandidates.Length, fuzzyNameTime.ElapsedMilliseconds)
    let callTime (call: CzPttModel.OperationalCall) =
        call.arrivalSeconds |> Option.orElse call.departureSeconds
    let coordinateForCall
            (selected: Map<string * string, SelectedCoordinate>)
            (call: CzPttModel.OperationalCall) =
        Map.tryFind (call.countryCode, call.primaryCode) selected
    let serviceDaysByPa =
        messages
        |> Seq.map (fun message ->
            paId message,
            message.CzpttInformation.PlannedCalendar.BitmapDays
            |> Seq.filter ((=) '1')
            |> Seq.length)
        |> Map
    let passengerPointIdentities =
        rawResult.operationalCalls
        |> Seq.filter (fun call ->
            call.passengerCall && call.generatedTripIds.Length > 0)
        |> Seq.map (fun call -> call.countryCode, call.primaryCode)
        |> Set
    let estimateCandidates
            (identity: string * string)
            (calls: CzPttModel.OperationalCall array)
            index =
        let currentCall = calls.[index]
        match callTime currentCall with
        | None -> [||]
        | Some current ->
            let anchorAt requirePassenger candidateIndex =
                let call = calls.[candidateIndex]
                if requirePassenger && not call.passengerCall then None
                else
                    match coordinateForCall selectedCoordinates call,
                          callTime call with
                    | Some coordinate, Some time ->
                        Some (candidateIndex, coordinate, time)
                    | _ -> None
            let anchors requirePassenger =
                let previous =
                    seq { index - 1 .. -1 .. 0 }
                    |> Seq.tryPick (anchorAt requirePassenger)
                let next =
                    seq { index + 1 .. calls.Length - 1 }
                    |> Seq.tryPick (anchorAt requirePassenger)
                previous, next
            let serviceDays =
                Map.tryFind currentCall.paId serviceDaysByPa
                |> Option.defaultValue 0
            let candidates
                    twoSidedTier oneSidedTier
                    (previous:
                        (int * SelectedCoordinate * int) option,
                     next:
                        (int * SelectedCoordinate * int) option)
                    : EstimateCandidate array =
                match previous, next with
                | Some (leftIndex, left, leftTime),
                  Some (rightIndex, right, rightTime)
                    when rightTime > leftTime ->
                    let ratio =
                        Math.Clamp(
                            float (current - leftTime)
                            / float (rightTime - leftTime),
                            0.,
                            1.)
                    [|
                        {
                            identity = identity
                            latitude =
                                left.latitude
                                + (right.latitude - left.latitude) * ratio
                            longitude =
                                left.longitude
                                + (right.longitude - left.longitude) * ratio
                            methodName = "route_time"
                            anchorTier = twoSidedTier
                            callSpan = rightIndex - leftIndex
                            anchorDistance =
                                distanceMeters
                                    (left.latitude, left.longitude)
                                    (right.latitude, right.longitude)
                            serviceDays = serviceDays
                            paId = currentCall.paId
                            sourceSequence = currentCall.sourceSequence
                        }
                    |]
                | None, Some (anchorIndex, anchor, _) ->
                    let north =
                        300. * float (anchorIndex - index) / 111320.
                    [|
                        {
                            identity = identity
                            latitude = anchor.latitude + north
                            longitude = anchor.longitude
                            methodName = "route_end_north"
                            anchorTier = oneSidedTier
                            callSpan = anchorIndex - index
                            anchorDistance = 0.
                            serviceDays = serviceDays
                            paId = currentCall.paId
                            sourceSequence = currentCall.sourceSequence
                        }
                    |]
                | Some (anchorIndex, anchor, _), None ->
                    let north =
                        300. * float (index - anchorIndex) / 111320.
                    [|
                        {
                            identity = identity
                            latitude = anchor.latitude + north
                            longitude = anchor.longitude
                            methodName = "route_end_north"
                            anchorTier = oneSidedTier
                            callSpan = index - anchorIndex
                            anchorDistance = 0.
                            serviceDays = serviceDays
                            paId = currentCall.paId
                            sourceSequence = currentCall.sourceSequence
                        }
                    |]
                | _ -> [||]
            Array.concat [|
                anchors true |> candidates 0 2
                anchors false |> candidates 1 3
            |]
    let estimates =
        resolutionOrder
        |> Seq.filter (fun identity ->
            Set.contains identity passengerPointIdentities
            && not (Map.containsKey identity selectedCoordinates))
        |> Seq.collect (fun identity ->
            occurrencesByIdentity
            |> Map.tryFind identity
            |> Option.defaultValue [||]
            |> Seq.collect (fun (calls, index) ->
                estimateCandidates identity calls index))
        |> Seq.groupBy (fun candidate -> candidate.identity)
        |> Seq.choose (fun (identity, candidates) ->
            let values = candidates |> Seq.toArray
            if values.Length = 0 then None
            else
                let selected =
                    values
                    |> Array.minBy (fun candidate ->
                        candidate.anchorTier,
                        candidate.callSpan,
                        candidate.anchorDistance,
                        -candidate.serviceDays,
                        candidate.paId,
                        candidate.sourceSequence)
                Some (
                    identity,
                    {
                        latitude = selected.latitude
                        longitude = selected.longitude
                        source = "estimated"
                        objectId = None
                        matchMethod = selected.methodName
                    }))
        |> Map
    selectedCoordinates <-
        estimates
        |> Map.fold (fun selected identity coordinate ->
            Map.add identity coordinate selected) selectedCoordinates
    let resolvedPointIdentities =
        selectedCoordinates |> Map.keys |> Set
    let unresolvedPointIdentities =
        usedPointIdentities
        |> Array.filter (fun identity -> not (Set.contains identity resolvedPointIdentities))
    let unresolvedByCountry =
        unresolvedPointIdentities
        |> Seq.groupBy fst
        |> Seq.map (fun (countryCode, identities) ->
            let points = identities |> Seq.toArray
            let pointSet = points |> Set
            let summary: CzPttModel.CoordinateCountrySummary = {
                countryCode = countryCode
                pointCount = points.Length
                stopCount =
                    pointIdentityByStopId
                    |> Map.values
                    |> Seq.filter (fun identity -> Set.contains identity pointSet)
                    |> Seq.length
            }
            summary)
        |> Seq.sortBy (fun summary -> summary.countryCode)
        |> Seq.toArray
    let usedConflictingSr70Codes =
        let usedCzechCodes =
            usedPointIdentities
            |> Seq.choose (fun (country, code) ->
                if country = "CZ" then Some code else None)
            |> Set
        sr70.conflictingCodes
        |> Array.filter (fun code -> Set.contains code usedCzechCodes)
    let invalidSr70CodeSet = sr70.invalidCodes |> Set
    let diagnostic value =
        let country, code = value
        let coordinate = selectedCoordinates.[value]
        let result: CzPttModel.CoordinateResolutionDiagnostic = {
            sourceLocationId = $"{country}:{code}"
            countryCode = country
            primaryCode = code
            coordinateSource = coordinate.source
            coordinateSourceObjectId = coordinate.objectId
            coordinateMatchMethod = coordinate.matchMethod
        }
        result
    let coordinateDiagnostics: CzPttModel.CoordinateDiagnostics = {
        resolutionMethod = "sr70-authoritative-then-osm-then-route-estimate"
        resolvedPointCount = resolvedPointIdentities.Count
        resolvedStopCount =
            pointIdentityByStopId
            |> Map.values
            |> Seq.filter (fun identity -> Set.contains identity resolvedPointIdentities)
            |> Seq.length
        unresolvedByCountry = unresolvedByCountry
        unresolvedPointIds =
            unresolvedPointIdentities
            |> Array.map (fun (country, code) ->
                $"czptt:stop:{Uri.EscapeDataString(country)}:" +
                $"{Uri.EscapeDataString(code)}")
        unresolvedPassengerPointIds =
            unresolvedPointIdentities
            |> Array.filter (fun identity ->
                Set.contains identity passengerPointIdentities)
            |> Array.map (fun (country, code) ->
                $"czptt:stop:{Uri.EscapeDataString(country)}:" +
                $"{Uri.EscapeDataString(code)}")
        conflictingSr70Codes = usedConflictingSr70Codes
        authoritativeSr70Resolutions =
            resolvedPointIdentities
            |> Set.toArray
            |> Array.filter (fun identity ->
                selectedCoordinates.[identity].source = "sz_sr70")
            |> Array.map diagnostic
        osmGapFills =
            resolvedPointIdentities
            |> Set.toArray
            |> Array.filter (fun identity ->
                selectedCoordinates.[identity].source = "osm")
            |> Array.map diagnostic
        estimatedResolutions =
            resolvedPointIdentities
            |> Set.toArray
            |> Array.filter (fun identity ->
                selectedCoordinates.[identity].source = "estimated")
            |> Array.map diagnostic
        osmSr70Disagreements = conflicts.ToArray()
        invalidSr70Identities =
            usedPointIdentities
            |> Array.choose (fun (country, code) ->
                if country = "CZ" && Set.contains code invalidSr70CodeSet
                then Some $"{country}:{code}"
                else None)
        ambiguousOsmCandidates = ambiguous |> Seq.sort |> Seq.toArray
        corridorRejectedOsmCandidates =
            corridorRejected |> Seq.sort |> Seq.toArray
    }
    progress "index-stop-times" "completed"
    let unresolvedTimedOnlyIdentities =
        unresolvedPointIdentities
        |> Seq.filter (fun identity ->
            not (Set.contains identity passengerPointIdentities))
        |> Set
    let droppedStopIds =
        pointIdentityByStopId
        |> Map.toSeq
        |> Seq.choose (fun (stopId, identity) ->
            if Set.contains identity unresolvedTimedOnlyIdentities
            then Some stopId
            else None)
        |> Set
    let retainedStopTimes =
        rawResult.feed.stopTimes
        |> Array.filter (fun stopTime ->
            not (Set.contains stopTime.stopId droppedStopIds))
    let retainedStopTimeKeys =
        retainedStopTimes
        |> Seq.map (fun stopTime -> stopTime.tripId, stopTime.stopSequence)
        |> Set
    let directlyRetainedStopIds =
        retainedStopTimes |> Seq.map (fun stopTime -> stopTime.stopId) |> Set
    let retainedParentIds =
        rawResult.feed.stops
        |> Seq.choose (fun stop ->
            if Set.contains stop.id directlyRetainedStopIds
            then stop.parentStation
            else None)
        |> Set
    let retainedStops =
        rawResult.feed.stops
        |> Array.filter (fun stop ->
            Set.contains stop.id directlyRetainedStopIds
            || Set.contains stop.id retainedParentIds)
        |> Array.map (fun stop ->
            match Map.tryFind stop.id pointIdentityByStopId
                  |> Option.bind (fun identity ->
                      Map.tryFind identity selectedCoordinates) with
            | Some coordinate ->
                { stop with
                    name =
                        if coordinate.source = "estimated"
                        then Gtfs.markApproximateStopName stop.name
                        else stop.name
                    lat = Some (decimal coordinate.latitude)
                    lon = Some (decimal coordinate.longitude) }
            | None -> stop)
    let retainedTripStopZones =
        rawResult.feed.czTripStopZones
        |> Option.map (Array.filter (fun value ->
            Set.contains (value.tripId, value.stopSequence) retainedStopTimeKeys))
    let retainedOperationalCalls =
        rawResult.operationalCalls
        |> Array.map (fun call ->
            if Set.contains
                    (call.countryCode, call.primaryCode)
                    unresolvedTimedOnlyIdentities
            then { call with generatedTripIds = [||] }
            else call)
    let result = {
        rawResult with
            feed = {
                rawResult.feed with
                    stops = retainedStops
                    stopTimes = retainedStopTimes
                    czTripStopZones = retainedTripStopZones
            }
            operationalCalls = retainedOperationalCalls
            mergeDiagnostics =
                merger.UnknownCancellationTargets
                |> Seq.map (fun pair ->
                    $"unknown cancellation target {pair.Key} ({pair.Value} messages)")
                |> Seq.sort
                |> Seq.toArray
            cancelledPaIds = cancelledPaIds
            coordinateDiagnostics = coordinateDiagnostics
    }
    progress "prepare-sidecars" "started"
    let operationalCallsByPa =
        result.operationalCalls
        |> Seq.groupBy (fun call -> call.paId)
        |> Seq.map (fun (identity, calls) ->
            identity,
            calls |> Seq.sortBy (fun call -> call.sourceSequence) |> Seq.toArray)
        |> Map
    let generatedTripsByPa =
        operationalCallsByPa
        |> Map.map (fun _ calls ->
            calls
            |> Seq.collect (fun call -> call.generatedTripIds)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toArray)
    let acceptedPaIds = HashSet<string>(result.acceptedPaIds)
    progress "prepare-sidecars" "completed"

    progress "write-point-sidecar" "started"
    let pointRows =
        result.operationalCalls
        |> Seq.groupBy (fun call -> call.countryCode, call.primaryCode)
        |> Seq.map (fun (_, calls) -> calls |> Seq.minBy (fun call -> call.name))
        |> Seq.sortBy (fun call -> call.countryCode, call.primaryCode)
        |> Seq.map (fun call ->
            let sourceLocationId = $"{call.countryCode}:{call.primaryCode}"
            let coordinate =
                Map.tryFind (call.countryCode, call.primaryCode) selectedCoordinates
            row [
                "source_location_id", box sourceLocationId
                "country_code", box call.countryCode
                "primary_code", box call.primaryCode
                "source_name", box call.name
                "latitude",
                    nullableObj (coordinate |> Option.map (fun value -> value.latitude))
                "longitude",
                    nullableObj (coordinate |> Option.map (fun value -> value.longitude))
                "coordinate_source",
                    nullableObj (coordinate |> Option.map (fun value -> value.source))
                "coordinate_source_object_id",
                    nullableObj (coordinate |> Option.bind (fun value -> value.objectId))
                "coordinate_match_method",
                    nullableObj (coordinate |> Option.map (fun value -> value.matchMethod))
            ])
    writeParquet
        (Path.Combine(outputDirectory, "operational_points.parquet"))
        (table [|
            field<string> "source_location_id" false
            field<string> "country_code" false
            field<string> "primary_code" false
            field<string> "source_name" false
            field<double> "latitude" true
            field<double> "longitude" true
            field<string> "coordinate_source" true
            field<string> "coordinate_source_object_id" true
            field<string> "coordinate_match_method" true
        |] pointRows)
    progress "write-point-sidecar" "completed"

    progress "write-call-sidecar" "started"
    let callRows =
        result.operationalCalls
        |> Seq.sortBy (fun call -> call.paId, call.sourceSequence)
        |> Seq.map (fun call ->
            row [
                "source_pa_id", box call.paId
                "source_sequence", box call.sourceSequence
                "source_location_id", box $"{call.countryCode}:{call.primaryCode}"
                "passenger_call", box call.passengerCall
                "arrival_seconds", nullableObj call.arrivalSeconds
                "departure_seconds", nullableObj call.departureSeconds
                "subsidiary_code", nullableObj call.subsidiaryCode
                "subsidiary_name", nullableObj call.subsidiaryName
                "active_line_code", nullableObj call.activeLineCode
            ])
    writeParquet
        (Path.Combine(outputDirectory, "operational_calls.parquet"))
        (table [|
            field<string> "source_pa_id" false
            field<int> "source_sequence" false
            field<string> "source_location_id" false
            field<bool> "passenger_call" false
            field<int> "arrival_seconds" true
            field<int> "departure_seconds" true
            field<string> "subsidiary_code" true
            field<string> "subsidiary_name" true
            field<string> "active_line_code" true
        |] callRows)
    progress "write-call-sidecar" "completed"

    progress "write-call-projection" "started"
    let callProjectionRows =
        result.operationalCalls
        |> Seq.collect (fun call ->
            call.generatedTripIds
            |> Seq.map (fun tripId ->
                row [
                    "gtfs_trip_id", box tripId
                    "stop_sequence", box call.sourceSequence
                    "source_pa_id", box call.paId
                    "source_sequence", box call.sourceSequence
                ]))
        |> Seq.sortBy (fun value ->
            string value.["gtfs_trip_id"], unbox<int> value.["stop_sequence"])
    writeParquet
        (Path.Combine(outputDirectory, "source_call_metadata.parquet"))
        (table [|
            field<string> "gtfs_trip_id" false
            field<int> "stop_sequence" false
            field<string> "source_pa_id" false
            field<int> "source_sequence" false
        |] callProjectionRows)
    progress "write-call-projection" "completed"

    progress "write-note-metadata" "started"
    let noteRows =
        result.notes
        |> Seq.collect (fun note ->
            let trips = if note.tripIds.Length = 0 then [| None |] else note.tripIds |> Array.map Some
            trips |> Seq.map (fun trip ->
                row [
                    "source_note_id", box note.id
                    "source_pa_id", box note.paId
                    "note_kind", box note.kind
                    "source_code", nullableObj note.code
                    "gtfs_trip_id", nullableObj trip
                    "label", nullableObj note.label
                    "raw_value", box note.rawValue
                    "valid_from", nullableObj (note.validFrom |> Option.map string)
                    "valid_to", nullableObj (note.validTo |> Option.map string)
                    "resolved", box note.resolved
                    "from_sequence", nullableObj note.firstSequence
                    "to_sequence", nullableObj note.lastSequence
                ]))
        |> Seq.sortBy (fun value ->
            string value.["source_note_id"],
            (if isNull value.["gtfs_trip_id"] then "" else string value.["gtfs_trip_id"]))
    writeParquet
        (Path.Combine(outputDirectory, "source_note_metadata.parquet"))
        (table [|
            field<string> "source_note_id" false
            field<string> "source_pa_id" false
            field<string> "note_kind" false
            field<string> "source_code" true
            field<string> "gtfs_trip_id" true
            field<string> "label" true
            field<string> "raw_value" false
            field<string> "valid_from" true
            field<string> "valid_to" true
            field<bool> "resolved" false
            field<int> "from_sequence" true
            field<int> "to_sequence" true
        |] noteRows)
    progress "write-note-metadata" "completed"

    progress "write-feature-metadata" "started"
    let featureRows =
        result.features
        |> Seq.sortBy (fun feature -> feature.id)
        |> Seq.map (fun feature ->
            row [
                "source_feature_id", box feature.id
                "gtfs_trip_id", box feature.tripId
                "call_sequence", nullableObj feature.callSequence
                "source_code", box feature.sourceCode
                "feature_kind", box feature.kind
                "note_id", nullableObj feature.noteId
                "source_object_id", box feature.sourceObjectId
            ])
    writeParquet
        (Path.Combine(outputDirectory, "source_feature_metadata.parquet"))
        (table [|
            field<string> "source_feature_id" false
            field<string> "gtfs_trip_id" false
            field<int> "call_sequence" true
            field<string> "source_code" false
            field<string> "feature_kind" false
            field<string> "note_id" true
            field<string> "source_object_id" false
        |] featureRows)
    progress "write-feature-metadata" "completed"

    progress "write-ids-sidecar" "started"
    let idsRows =
        messages
        |> Seq.filter (fun message -> acceptedPaIds.Contains(paId message))
        |> Seq.collect (fun message ->
            parameterPairs message.NetworkSpecificParameter
            |> Seq.filter (fun (name, _) ->
                name = "CZIPTS" || name = "CZCalendarIPTS")
            |> Seq.mapi (fun index (name, value) ->
                let fields = value.Split('|')
                let sourceCode =
                    if name = "CZIPTS" then fields |> Array.tryItem 0
                    else None
                let calendarId =
                    if name = "CZIPTS" then fields |> Array.tryItem 5
                    else fields |> Array.tryItem 0
                let coverageId = $"{paId message}:{index + 1}"
                row [
                    "source_coverage_id", box coverageId
                    "source_pa_id", box (paId message)
                    "source_sequence", box (index + 1)
                    "record_type", box name
                    "source_code", nullableObj sourceCode
                    "ids_system_id",
                        nullableObj (sourceCode |> Option.bind (idsSystem catalog))
                    "coverage_role",
                        nullableObj (
                            if name = "CZIPTS" then
                                sourceCode |> Option.bind (idsRole catalog)
                            else Some "calendar")
                    "from_location_code",
                        nullableObj (
                            if name = "CZIPTS" then fields |> Array.tryItem 1
                            else None)
                    "from_occurrence",
                        nullableObj (
                            if name = "CZIPTS" then fields |> Array.tryItem 2
                            else None)
                    "to_location_code",
                        nullableObj (
                            if name = "CZIPTS" then fields |> Array.tryItem 3
                            else None)
                    "to_occurrence",
                        nullableObj (
                            if name = "CZIPTS" then fields |> Array.tryItem 4
                            else None)
                    "calendar_id", nullableObj calendarId
                    "source_value", box value
                ]))
    writeParquet
        (Path.Combine(outputDirectory, "source_ids_coverage_metadata.parquet"))
        (table [|
            field<string> "source_coverage_id" false
            field<string> "source_pa_id" false
            field<int> "source_sequence" false
            field<string> "record_type" false
            field<string> "source_code" true
            field<string> "ids_system_id" true
            field<string> "coverage_role" true
            field<string> "from_location_code" true
            field<string> "from_occurrence" true
            field<string> "to_location_code" true
            field<string> "to_occurrence" true
            field<string> "calendar_id" true
            field<string> "source_value" false
        |] idsRows)
    progress "write-ids-sidecar" "completed"

    progress "write-ids-trip-projection" "started"
    let coverageTrips (pa: string) (name: string) (value: string) =
        let allTrips =
            Map.tryFind pa generatedTripsByPa |> Option.defaultValue [||]
        if name <> "CZIPTS" then allTrips
        else
            let fields = value.Split('|')
            let sourceCalls =
                Map.tryFind pa operationalCallsByPa |> Option.defaultValue [||]
            let matchingCalls (rawCode: string) =
                let compact = rawCode.Trim().ToUpperInvariant()
                sourceCalls
                |> Array.filter (fun call ->
                    compact = call.primaryCode.ToUpperInvariant()
                    || compact =
                       (call.countryCode + call.primaryCode).ToUpperInvariant())
            let occurrence (raw: string option) fallback values =
                let parsed =
                    raw
                    |> Option.bind (fun text ->
                        match Int32.TryParse(text) with
                        | true, number when number > 0 -> Some number
                        | _ -> None)
                    |> Option.defaultValue fallback
                values |> Array.tryItem (parsed - 1)
            let resolved =
                if fields.Length < 5 then None
                else
                    let fromCalls = matchingCalls fields.[1]
                    let toCalls = matchingCalls fields.[3]
                    match occurrence (fields |> Array.tryItem 2) 1 fromCalls,
                          occurrence (fields |> Array.tryItem 4) toCalls.Length toCalls with
                    | Some first, Some last when
                        first.sourceSequence <= last.sourceSequence ->
                        Some (first.sourceSequence, last.sourceSequence)
                    | _ -> None
            match resolved with
            | None -> [||]
            | Some (first, last) ->
                sourceCalls
                |> Array.filter (fun call ->
                    call.sourceSequence >= first && call.sourceSequence <= last)
                |> Array.collect (fun call -> call.generatedTripIds)
                |> Array.distinct
                |> Array.sort
    let idsTripRows =
        messages
        |> Seq.filter (fun message -> acceptedPaIds.Contains(paId message))
        |> Seq.collect (fun message ->
            parameterPairs message.NetworkSpecificParameter
            |> Seq.filter (fun (name, _) ->
                name = "CZIPTS" || name = "CZCalendarIPTS")
            |> Seq.mapi (fun index (name, value) ->
                $"{paId message}:{index + 1}", coverageTrips (paId message) name value)
            |> Seq.collect (fun (coverageId, tripIds) ->
                tripIds
                |> Seq.map (fun tripId ->
                    row [
                        "source_coverage_id", box coverageId
                        "gtfs_trip_id", box tripId
                    ])))
        |> Seq.sortBy (fun value ->
            string value.["source_coverage_id"], string value.["gtfs_trip_id"])
    writeParquet
        (Path.Combine(outputDirectory, "source_ids_coverage_trip_metadata.parquet"))
        (table [|
            field<string> "source_coverage_id" false
            field<string> "gtfs_trip_id" false
        |] idsTripRows)
    progress "write-ids-trip-projection" "completed"

    result
