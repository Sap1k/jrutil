// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// CZPTT coordinate sources: SR70, OSM railway candidates, reviewed aliases and name matching.
module JrUtil.CzPttCoordinates

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

type internal Sr70CoordinateIndex = {
    coordinates: Map<string * string, double * double>
    names: CzPttModel.PointNameIndex
    conflictingCodes: string array
    invalidCodes: string array
}

let internal loadSr70Coordinates path =
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

type internal OsmCandidate = {
    objectId: string
    countryCode: string option
    plc: string option
    names: string array
    latitude: double
    longitude: double
}

type internal SelectedCoordinate = {
    latitude: double
    longitude: double
    source: string
    objectId: string option
    matchMethod: string
}

type internal EstimateCandidate = {
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

let internal optionalTag name (node: Node) =
    let mutable value = null
    if not (isNull node.Tags) && node.Tags.TryGetValue(name, &value)
    then Option.ofObj value |> Option.filter (String.IsNullOrWhiteSpace >> not)
    else None

let private nonAlphanumericRun = Regex(@"[^\p{L}\p{N}]+", RegexOptions.Compiled)
let private trailingOperationalQualifier = Regex(@"\s*\([^)]*\)\s*$", RegexOptions.Compiled)
let private routeSectionToken = Regex(@"^R[0-9]+$", RegexOptions.Compiled)

let internal normalizedName (value: string) =
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
    let withoutMarks = StringBuilder(decomposed.Length)
    for character in decomposed do
        if Globalization.CharUnicodeInfo.GetUnicodeCategory(character)
           <> Globalization.UnicodeCategory.NonSpacingMark then
            withoutMarks.Append(character) |> ignore
    nonAlphanumericRun.Replace(withoutMarks.ToString(), " ")
        .Trim().ToUpperInvariant()

let private railwayNameSuffixTokens =
    Set.ofList [
        "Bf"; "Fbf"; "Gr"; "Hp"; "Hst"; "N"; "Nz"; "Pzs"; "S"; "St";
        "Z"; "Zast"; "Zastavka"
    ]
    |> Set.map normalizedName

let internal railwayNameCore (value: string) =
    let withoutOperationalQualifier =
        trailingOperationalQualifier.Replace(value, "")
    let tokens =
        (normalizedName withoutOperationalQualifier)
            .Split(' ', StringSplitOptions.RemoveEmptyEntries)
    let mutable kept = tokens.Length
    while kept > 0
          && (Set.contains tokens.[kept - 1] railwayNameSuffixTokens
              || routeSectionToken.IsMatch(tokens.[kept - 1])) do
        kept <- kept - 1
    if kept = 0 then normalizedName value
    else String.Join(" ", tokens, 0, kept)

let internal editSimilarity (left: string) (right: string) =
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

/// The normalized forms `nameSimilarity` and `fuzzyQualifierCompatible`
/// compare, computed once per name instead of once per compared pair.
type internal PreparedName = {
    full: string
    core: string
    compactCore: string
    tokens: string array
}

let internal prepareName (value: string) =
    let full = normalizedName value
    let core = railwayNameCore value
    {
        full = full
        core = core
        compactCore = core.Replace(" ", "")
        tokens = full.Split(' ', StringSplitOptions.RemoveEmptyEntries)
    }

/// Upper bound of `editSimilarity`: the edit distance is at least the length difference.
let private editSimilarityBound (left: string) (right: string) =
    if left = right then 1.
    elif String.IsNullOrEmpty(left) || String.IsNullOrEmpty(right) then 0.
    else
        1. - float (abs (left.Length - right.Length))
             / float (max left.Length right.Length)

/// `nameSimilarity` over prepared names; `None` when it is provably below `minimum`.
let internal preparedNameSimilarityAtLeast minimum (left: PreparedName) (right: PreparedName) =
    let fullBound = editSimilarityBound left.full right.full
    let coreBound = editSimilarityBound left.core right.core
    let compactBound = editSimilarityBound left.compactCore right.compactCore
    if max fullBound (max coreBound compactBound) < minimum then None
    else
        let score value bound (leftValue: string) rightValue =
            if bound < minimum || bound <= value then value
            else max value (editSimilarity leftValue rightValue)
        let full = if fullBound < minimum then 0. else editSimilarity left.full right.full
        let core = score full coreBound left.core right.core
        let best = score core compactBound left.compactCore right.compactCore
        if best < minimum then None else Some best

let internal preparedNameSimilarity (left: PreparedName) (right: PreparedName) =
    [|
        editSimilarity left.full right.full
        editSimilarity left.core right.core
        editSimilarity left.compactCore right.compactCore
    |]
    |> Array.max

let internal nameSimilarity (left: string) (right: string) =
    preparedNameSimilarity (prepareName left) (prepareName right)

let internal protectedFuzzyNameTokens =
    [
        "Ost"; "West"; "Nord"; "Süd"; "Mitte"
        "východ"; "západ"; "sever"; "jih"; "juh"; "střed"
        "Wschód"; "Zachód"; "Północ"; "Południe"; "Górna"; "Dolna"
        "město"; "mesto"; "miasto"; "centrum"
    ]
    |> Seq.map normalizedName
    |> Set

/// Protected qualifier tokens of a prepared expected name.
let internal requiredQualifierTokens (expected: PreparedName) =
    expected.tokens
    |> Array.filter (fun token -> Set.contains token protectedFuzzyNameTokens)

let internal preparedQualifierCompatible (required: string array) (candidate: PreparedName) =
    required
    |> Array.forall (fun requiredToken ->
        candidate.tokens
        |> Array.exists (fun candidateToken ->
            candidateToken = requiredToken
            || candidateToken.StartsWith(
                requiredToken,
                StringComparison.Ordinal)))

let internal fuzzyQualifierCompatible (expected: string) (candidate: string) =
    preparedQualifierCompatible
        (requiredQualifierTokens (prepareName expected))
        (prepareName candidate)

let internal normalizedCountryCode (value: string) =
    match value.Trim().ToUpperInvariant() with
    | "AUT" -> "AT"
    | "CZE" -> "CZ"
    | "DEU" -> "DE"
    | "POL" -> "PL"
    | "SVK" -> "SK"
    | country -> country

let internal loadOsmCandidates path =
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

let internal loadAliases path =
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

let internal distanceMeters (latitude1, longitude1) (latitude2, longitude2) =
    Geo.haversineMetres latitude1 longitude1 latitude2 longitude2
