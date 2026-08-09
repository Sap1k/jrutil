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

[<Literal>]
let ParquetSchemaVersion = 1

type private Table = {
    fields: DataField array
    rows: IReadOnlyCollection<IDictionary<string, obj>>
}

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
    rows = rows |> Seq.toArray :> IReadOnlyCollection<IDictionary<string, obj>>
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

let private idsRole (catalog: CzPttToGtfs.CatalogSnapshot) code =
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

let private idsSystem (catalog: CzPttToGtfs.CatalogSnapshot) code =
    catalog.ids
    |> Array.tryFind (fun record -> record.code = code)
    |> Option.map (fun record ->
        let parts = record.abbreviation.Split('_')
        if parts.Length > 1 then parts.[0] else record.abbreviation)

type private Sr70CoordinateIndex = {
    coordinates: Map<string * string, double * double>
    names: CzPttToGtfs.PointNameIndex
    conflictingCodes: string array
    invalidCodes: string array
}

let private loadSr70Coordinates path =
    match path with
    | None -> {
        coordinates = Map.empty
        names = Map.empty
        conflictingCodes = [||]
        invalidCodes = [||]
      }
    | Some file ->
        let parsed =
            CsvFile.Parse(File.ReadAllText(file), hasHeaders = false).Rows
            |> Seq.choose (fun row ->
                let fields = row.Columns
                if fields.Length < 1 || fields.[0].Length < 5 then None
                else
                    let code = fields.[0].Substring(0, 5)
                    let name =
                        if fields.Length < 2 then None
                        else
                            let value = fields.[1].Trim()
                            if String.IsNullOrWhiteSpace(value) || value = "-"
                            then None
                            else Some value
                    if fields.Length < 4 then Some (code, name, None)
                    else
                        match Double.TryParse(
                                  fields.[fields.Length - 2],
                                  NumberStyles.Float,
                                  CultureInfo.InvariantCulture),
                              Double.TryParse(
                                  fields.[fields.Length - 1],
                                  NumberStyles.Float,
                                  CultureInfo.InvariantCulture) with
                        | (true, latitude), (true, longitude)
                            when latitude >= -90. && latitude <= 90.
                                 && longitude >= -180. && longitude <= 180. ->
                            Some (code, name, Some (latitude, longitude))
                        | _ -> Some (code, name, None))
            |> Seq.toArray
        let grouped =
            parsed
            |> Seq.choose (fun (code, _, coordinates) ->
                coordinates |> Option.map (fun value -> code, value))
            |> Seq.groupBy fst
            |> Seq.map (fun (code, values) ->
                code, values |> Seq.map snd |> Seq.distinct |> Seq.toArray)
            |> Seq.toArray
        let invalidCodes =
            parsed
            |> Seq.choose (fun (code, _, coordinates) ->
                if coordinates.IsNone then Some code else None)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toArray
        {
            coordinates =
                grouped
                |> Seq.choose (fun (code, coordinates) ->
                    if coordinates.Length = 1
                       && not (Array.contains code invalidCodes)
                    then Some (("CZ", code), coordinates.[0])
                    else None)
                |> Map
            names =
                parsed
                |> Seq.choose (fun (code, name, _) ->
                    name |> Option.map (fun value -> code, value))
                |> Seq.groupBy fst
                |> Seq.choose (fun (code, values) ->
                    let names = values |> Seq.map snd |> Seq.distinct |> Seq.toArray
                    if names.Length = 1 then Some (("CZ", code), names.[0])
                    else None)
                |> Map
            conflictingCodes =
                grouped
                |> Seq.choose (fun (code, coordinates) ->
                    if coordinates.Length > 1 then Some code else None)
                |> Seq.sort
                |> Seq.toArray
            invalidCodes = invalidCodes
        }

type private OsmCandidate = {
    objectId: string
    countryCode: string option
    plc: string option
    names: string array
    latitude: double
    longitude: double
}

type private SelectedCoordinate = {
    latitude: double
    longitude: double
    source: string
    objectId: string option
    matchMethod: string
}

type private EstimateCandidate = {
    identity: string * string
    latitude: double
    longitude: double
    methodName: string
    anchorTier: int
    callSpan: int
    anchorDistance: double
    serviceDays: int
    paId: string
    sourceSequence: int
}

let private optionalTag name (node: Node) =
    let mutable value = null
    if not (isNull node.Tags) && node.Tags.TryGetValue(name, &value)
    then Option.ofObj value |> Option.filter (String.IsNullOrWhiteSpace >> not)
    else None

let private normalizedName (value: string) =
    let transliterated =
        value
            .Replace("ß", "ss")
            .Replace("ẞ", "SS")
            .Replace("Ł", "L")
            .Replace("ł", "l")
            .Replace("Ø", "O")
            .Replace("ø", "o")
            .Replace("Æ", "AE")
            .Replace("æ", "ae")
    let decomposed = transliterated.Normalize(NormalizationForm.FormD)
    let withoutMarks =
        decomposed
        |> Seq.filter (fun character ->
            Globalization.CharUnicodeInfo.GetUnicodeCategory(character)
            <> Globalization.UnicodeCategory.NonSpacingMark)
        |> Seq.toArray
        |> String
    Regex.Replace(withoutMarks, @"[^\p{L}\p{N}]+", " ")
        .Trim().ToUpperInvariant()

let private railwayNameCore (value: string) =
    let withoutOperationalQualifier =
        Regex.Replace(value, @"\s*\([^)]*\)\s*$", "")
    let removable =
        Set.ofList [
            "Bf"; "Fbf"; "Gr"; "Hp"; "Hst"; "N"; "Nz"; "Pzs"; "S"; "St";
            "Z"; "Zast"; "Zastavka"
        ]
        |> Set.map normalizedName
    let tokens =
        (normalizedName withoutOperationalQualifier)
            .Split(' ', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
    let rec trimSuffix values =
        match List.tryLast values with
        | Some token when
            Set.contains token removable
            || Regex.IsMatch(token, @"^R[0-9]+$") ->
            values |> List.take (values.Length - 1) |> trimSuffix
        | _ -> values
    match trimSuffix tokens with
    | [] -> normalizedName value
    | values -> String.concat " " values

let private editSimilarity (left: string) (right: string) =
    if left = right then 1.
    elif String.IsNullOrEmpty(left) || String.IsNullOrEmpty(right) then 0.
    else
        let previous = Array.init (right.Length + 1) id
        let current = Array.zeroCreate<int> (right.Length + 1)
        for leftIndex = 1 to left.Length do
            current.[0] <- leftIndex
            for rightIndex = 1 to right.Length do
                let substitution =
                    if left.[leftIndex - 1] = right.[rightIndex - 1] then 0 else 1
                current.[rightIndex] <-
                    min
                        (min
                            (current.[rightIndex - 1] + 1)
                            (previous.[rightIndex] + 1))
                        (previous.[rightIndex - 1] + substitution)
            Array.blit current 0 previous 0 current.Length
        1. - float previous.[right.Length] / float (max left.Length right.Length)

let private nameSimilarity (left: string) (right: string) =
    let leftFull = normalizedName left
    let rightFull = normalizedName right
    let leftCore = railwayNameCore left
    let rightCore = railwayNameCore right
    let compact (value: string) = value.Replace(" ", "")
    [|
        editSimilarity leftFull rightFull
        editSimilarity leftCore rightCore
        editSimilarity (compact leftCore) (compact rightCore)
    |]
    |> Array.max

let private protectedFuzzyNameTokens =
    [
        "Ost"; "West"; "Nord"; "Süd"; "Mitte"
        "východ"; "západ"; "sever"; "jih"; "juh"; "střed"
        "Wschód"; "Zachód"; "Północ"; "Południe"; "Górna"; "Dolna"
        "město"; "mesto"; "miasto"; "centrum"
    ]
    |> Seq.map normalizedName
    |> Set

let private fuzzyQualifierCompatible (expected: string) (candidate: string) =
    let tokens value =
        (normalizedName value).Split(
            ' ',
            StringSplitOptions.RemoveEmptyEntries)
    let required =
        tokens expected
        |> Array.filter (fun token -> Set.contains token protectedFuzzyNameTokens)
    let available = tokens candidate
    required
    |> Array.forall (fun requiredToken ->
        available
        |> Array.exists (fun candidateToken ->
            candidateToken = requiredToken
            || candidateToken.StartsWith(
                requiredToken,
                StringComparison.Ordinal)))

let private normalizedCountryCode (value: string) =
    match value.Trim().ToUpperInvariant() with
    | "AUT" -> "AT"
    | "CZE" -> "CZ"
    | "DEU" -> "DE"
    | "POL" -> "PL"
    | "SVK" -> "SK"
    | country -> country

let private loadOsmCandidates path =
    match path with
    | None -> [||]
    | Some file ->
        use stream = File.OpenRead(file)
        new PBFOsmStreamSource(stream)
        |> Seq.choose (function
            | :? Node as node
                when node.Id.HasValue
                     && node.Latitude.HasValue
                     && node.Longitude.HasValue ->
                let railway = optionalTag "railway" node
                let publicTransport = optionalTag "public_transport" node
                let isRailwayLocation =
                    railway
                    |> Option.exists (fun value ->
                        value = "station" || value = "halt" || value = "stop")
                    || publicTransport
                       |> Option.exists (fun value ->
                           value = "station" || value = "stop_position")
                if not isRailwayLocation then None
                else
                    let plc =
                        optionalTag "ref:EU:PLC" node
                        |> Option.bind (fun value ->
                            let compact =
                                Regex.Replace(value, @"\s+", "").ToUpperInvariant()
                            if Regex.IsMatch(compact, @"^[A-Z]{2}[0-9]{5}$")
                            then Some compact
                            else None)
                    let country =
                        plc
                        |> Option.map (fun value -> value.Substring(0, 2))
                        |> Option.orElseWith (fun () ->
                            optionalTag "addr:country" node
                            |> Option.orElseWith (fun () ->
                                optionalTag "is_in:country_code" node)
                            |> Option.map normalizedCountryCode)
                    let names =
                        [|
                            optionalTag "name" node
                            optionalTag "official_name" node
                            optionalTag "alt_name" node
                            optionalTag "short_name" node
                            optionalTag "loc_name" node
                            optionalTag "old_name" node
                            optionalTag "uic_name" node
                            optionalTag "name:cs" node
                            optionalTag "name:de" node
                            optionalTag "name:pl" node
                            optionalTag "name:sk" node
                        |]
                        |> Array.choose id
                        |> Array.map (fun value -> value.Trim())
                        |> Array.filter (String.IsNullOrWhiteSpace >> not)
                        |> Array.distinct
                    Some {
                        objectId = $"osm:node:{node.Id.Value}"
                        countryCode = country
                        plc = plc
                        names = names
                        latitude = node.Latitude.Value
                        longitude = node.Longitude.Value
                    }
            | _ -> None)
        |> Seq.sortBy (fun value -> value.objectId)
        |> Seq.toArray

let private loadAliases path =
    match path with
    | None -> Map.empty
    | Some file when not (File.Exists(file)) -> Map.empty
    | Some file ->
        use document = JsonDocument.Parse(File.ReadAllText(file))
        let root =
            if document.RootElement.ValueKind <> JsonValueKind.Object then
                document.RootElement
            else
                match document.RootElement.TryGetProperty("aliases") with
                | true, value -> value
                | _ -> document.RootElement
        if root.ValueKind <> JsonValueKind.Object then Map.empty
        else
            root.EnumerateObject()
            |> Seq.choose (fun property ->
                if property.Value.ValueKind = JsonValueKind.String
                then property.Value.GetString() |> Option.ofObj
                     |> Option.map (fun value -> property.Name, value)
                else None)
            |> Map

let private distanceMeters (latitude1, longitude1) (latitude2, longitude2) =
    let radians value = value * Math.PI / 180.
    let dLatitude = radians (latitude2 - latitude1)
    let dLongitude = radians (longitude2 - longitude1)
    let a =
        Math.Sin(dLatitude / 2.) ** 2.
        + Math.Cos(radians latitude1) * Math.Cos(radians latitude2)
          * Math.Sin(dLongitude / 2.) ** 2.
    6371000. * 2. * Math.Atan2(Math.Sqrt(a), Math.Sqrt(1. - a))

let writeSidecarsWithProgressAndOptions catalog options inputPath outputDirectory
                                       sr70Path _sr70Name20Path osmPath
                                       osmAliasesPath
                                       (progress: string -> string -> unit) =
    Directory.CreateDirectory(outputDirectory) |> ignore
    progress "parse-input" "started"
    let merger = CzPttMerge.CzPttMerger()
    let parsed = CzPtt.parseAll inputPath |> Seq.toArray
    progress "parse-input" "completed"
    progress "merge-messages" "started"
    parsed |> merger.ProcessAll
    progress "merge-messages" "completed"
    progress "prepare-identities" "started"
    let sourcePaIds =
        parsed
        |> Array.choose (fun (_, message) ->
            match message with
            | CzPtt.Timetable timetable -> Some (paId timetable)
            | _ -> None)
        |> Set
    let messages =
        merger.Messages.Values |> Seq.sortBy paId |> Seq.toArray
    let survivingPaIds = messages |> Seq.map paId |> Set
    let cancelledPaIds =
        Set.difference sourcePaIds survivingPaIds |> Set.toArray |> Array.sort
    progress "prepare-identities" "completed"
    progress "load-coordinate-sources" "started"
    let sr70 = loadSr70Coordinates sr70Path
    let osmCandidates = loadOsmCandidates osmPath
    let osmAliases = loadAliases osmAliasesPath
    progress "load-coordinate-sources" "completed"
    progress "convert-gtfs" "started"
    let rawResult =
        CzPttToGtfs.convertWithPointNamesAndOptions
            catalog options sr70.names messages
    progress "convert-gtfs" "completed"
    progress "index-stop-times" "started"
    let pointIdentityByStopId =
        rawResult.operationalCalls
        |> Seq.filter (fun call -> call.generatedTripIds.Length > 0)
        |> Seq.collect (fun call ->
            seq {
                call.generatedStationId, (call.countryCode, call.primaryCode)
                call.generatedStopId, (call.countryCode, call.primaryCode)
            })
        |> Seq.distinct
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
        ResizeArray<CzPttToGtfs.CoordinateConflictDiagnostic>()
    let osmByPlcIndex =
        osmCandidates
        |> Seq.choose (fun candidate ->
            candidate.plc |> Option.map (fun plc -> plc, candidate))
        |> Seq.groupBy fst
        |> Seq.map (fun (plc, values) ->
            plc, values |> Seq.map snd |> Seq.toArray)
        |> Map
    let osmByObjectId =
        osmCandidates
        |> Seq.groupBy (fun candidate -> candidate.objectId)
        |> Seq.map (fun (objectId, values) -> objectId, values |> Seq.toArray)
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
    let journeys: CzPttToGtfs.OperationalCall array array =
        rawResult.operationalCalls
        |> Seq.groupBy (fun call -> call.paId)
        |> Seq.map (fun (_, calls) ->
            calls |> Seq.sortBy (fun call -> call.sourceSequence) |> Seq.toArray)
        |> Seq.toArray
    let occurrencesByIdentity:
            Map<string * string, (CzPttToGtfs.OperationalCall array * int) array> =
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
    let fuzzyNameCandidates (identity: string * string) =
        match fuzzyNameCache.TryGetValue(identity) with
        | true, values -> values
        | _ ->
            let expectedNames =
                Map.tryFind identity identityNames
                |> Option.defaultValue Set.empty
                |> Set.toArray
            let values =
                osmCandidates
                |> Array.choose (fun candidate ->
                    let score =
                        candidate.names
                        |> Array.collect (fun candidateName ->
                            expectedNames
                            |> Array.choose (fun expectedName ->
                                if fuzzyQualifierCompatible
                                       expectedName candidateName
                                then
                                    Some (
                                        nameSimilarity
                                            expectedName candidateName)
                                else None))
                        |> fun scores ->
                            if scores.Length = 0 then 0. else Array.max scores
                    if score >= 0.58 then Some (score, candidate) else None)
                |> Array.sortBy (fun (score, candidate) ->
                    -score, candidate.objectId)
                |> Array.truncate 32
                |> Array.map snd
            fuzzyNameCache.Add(identity, values)
            values
    let candidateGroups (identity: string * string) =
        let country, code = identity
        let alias =
            osmAliases
            |> Map.tryFind $"{country}:{code}"
            |> Option.bind (fun expected -> Map.tryFind expected osmByObjectId)
            |> Option.defaultValue [||]
        [|
            "ref_eu_plc", fun () -> osmByPlc identity
            "reviewed_alias", fun () -> alias
            "normalized_exact_name", fun () -> exactNameCandidates identity
            "normalized_railway_name", fun () -> coreNameCandidates identity
            "normalized_fuzzy_name", fun () -> fuzzyNameCandidates identity
        |]
    let departureTime (call: CzPttToGtfs.OperationalCall) =
        call.departureSeconds |> Option.orElse call.arrivalSeconds
    let arrivalTime (call: CzPttToGtfs.OperationalCall) =
        call.arrivalSeconds |> Option.orElse call.departureSeconds
    let edgePlausibility
            (leftCoordinate: double * double)
            (leftCall: CzPttToGtfs.OperationalCall)
            (rightCoordinate: double * double)
            (rightCall: CzPttToGtfs.OperationalCall) =
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
    let callTime (call: CzPttToGtfs.OperationalCall) =
        call.arrivalSeconds |> Option.orElse call.departureSeconds
    let coordinateForCall
            (selected: Map<string * string, SelectedCoordinate>)
            (call: CzPttToGtfs.OperationalCall) =
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
            (calls: CzPttToGtfs.OperationalCall array)
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
            let summary: CzPttToGtfs.CoordinateCountrySummary = {
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
        let result: CzPttToGtfs.CoordinateResolutionDiagnostic = {
            sourceLocationId = $"{country}:{code}"
            countryCode = country
            primaryCode = code
            coordinateSource = coordinate.source
            coordinateSourceObjectId = coordinate.objectId
            coordinateMatchMethod = coordinate.matchMethod
        }
        result
    let coordinateDiagnostics: CzPttToGtfs.CoordinateDiagnostics = {
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

let writeSidecarsWithProgress catalog mode inputPath outputDirectory sr70Path
                              sr70Name20Path osmPath osmAliasesPath progress =
    writeSidecarsWithProgressAndOptions
        catalog {
            operationalPointMode = mode
            blockMode = CzPttToGtfs.Blocks
        } inputPath outputDirectory sr70Path sr70Name20Path osmPath
        osmAliasesPath progress

let writeSidecars catalog mode inputPath outputDirectory sr70Path sr70Name20Path
                  osmPath osmAliasesPath =
    writeSidecarsWithProgress
        catalog mode inputPath outputDirectory sr70Path sr70Name20Path
        osmPath osmAliasesPath
        (fun _ _ -> ())

let writeSidecarsWithOptions catalog options inputPath outputDirectory sr70Path
                             sr70Name20Path osmPath osmAliasesPath =
    writeSidecarsWithProgressAndOptions
        catalog options inputPath outputDirectory sr70Path sr70Name20Path
        osmPath osmAliasesPath (fun _ _ -> ())

let writeManifest outputDirectory =
    let textRows path =
        File.ReadLines(path)
        |> Seq.skip 1
        |> Seq.filter (String.IsNullOrWhiteSpace >> not)
        |> Seq.length
    let parquetRows path =
        use stream = File.OpenRead(path)
        let reader = ParquetReader.CreateAsync(stream).GetAwaiter().GetResult()
        let rows =
            Int32.Parse(
                reader.CustomMetadata.["obehy.row_count"],
                CultureInfo.InvariantCulture)
        reader.DisposeAsync().AsTask().GetAwaiter().GetResult()
        rows
    let files =
        Directory.EnumerateFiles(outputDirectory, "*", SearchOption.AllDirectories)
        |> Seq.filter (fun path ->
            Path.GetFileName(path) <> "manifest.json")
        |> Seq.sort
        |> Seq.map (fun path ->
            use stream = File.OpenRead(path)
            let digest = SHA256.HashData(stream) |> Convert.ToHexString
            let value = Dictionary<string, obj>()
            value.["path"] <-
                box (
                    Path.GetRelativePath(outputDirectory, path)
                        .Replace(Path.DirectorySeparatorChar, '/'))
            value.["bytes"] <- box (FileInfo(path).Length)
            value.["sha256"] <- box (digest.ToLowerInvariant())
            value.["rows"] <-
                box (
                    match Path.GetExtension(path).ToLowerInvariant() with
                    | ".parquet" -> Nullable (parquetRows path)
                    | ".txt" -> Nullable (textRows path)
                    | _ -> Nullable<int>())
            value :> obj)
        |> Seq.toArray
    let manifest = Dictionary<string, obj>()
    manifest.["schema_version"] <- box 1
    manifest.["bundle_format"] <- box "czptt-v1"
    manifest.["parquet_schema_version"] <- box ParquetSchemaVersion
    manifest.["source_format"] <- box "czptt"
    manifest.["files"] <- box files
    File.WriteAllText(
        Path.Combine(outputDirectory, "manifest.json"),
        JsonSerializer.Serialize(
            manifest,
            JsonSerializerOptions(WriteIndented = true)) + "\n")
