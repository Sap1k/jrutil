// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023-2026 David Koňařík and contributors

/// CZPTT conversion options, catalog snapshots, results and internal call model.
module JrUtil.CzPttModel

open System
open System.Globalization
open System.Text.Json
open System.Text.RegularExpressions
open FSharp.Data
open NodaTime
open NodaTime.Text
open Serilog
open JrUtil.CzPtt
open JrUtil.CzPttMerge
open JrUtil.GtfsModel
open JrUtil.Utils

type OperationalPointMode =
    | Gtfs
    | Sidecar

type ConversionOptions = {
    operationalPointMode: OperationalPointMode
}

type CatalogLine = {
    code: string
    mark: string
    name: string
    validFrom: LocalDate option
    validTo: LocalDate option
}

type CatalogCompany = {
    code: string
    name: string
    url: string option
}

type CatalogIds = {
    code: string
    abbreviation: string
    name: string
    note: string option
    validFrom: LocalDate option
    validTo: LocalDate option
}

type CatalogCode = {
    code: string
    abbreviation: string
}

type CatalogNote = {
    code: string
    name: string
    text: string option
    validFrom: LocalDate option
    validTo: LocalDate option
}

type CatalogSnapshot = {
    lines: CatalogLine array
    companies: CatalogCompany array
    ids: CatalogIds array
    trainTypes: CatalogCode array
    commercialTrainTypes: CatalogCode array
    centralNotes: CatalogNote array
}

type PointNameIndex = Map<string * string, string>

type RejectedJourney = {
    paId: string
    reason: string
    sequence: int option
    previousSeconds: int option
    currentSeconds: int option
}

type OperationalCall = {
    paId: string
    sourceSequence: int
    countryCode: string
    primaryCode: string
    name: string
    passengerCall: bool
    arrivalSeconds: int option
    departureSeconds: int option
    subsidiaryCode: string option
    subsidiaryName: string option
    activeLineCode: string option
    generatedStationId: string
    generatedStopId: string
    generatedTripIds: string array
}

type CoordinateCountrySummary = {
    countryCode: string
    pointCount: int
    stopCount: int
}

type BoundaryAdjustment = {
    paId: string
    sourceSequence: int
    appliedSequence: int option
    reason: string
}

type CoordinateResolutionDiagnostic = {
    sourceLocationId: string
    countryCode: string
    primaryCode: string
    coordinateSource: string
    coordinateSourceObjectId: string option
    coordinateMatchMethod: string
}

type CoordinateConflictDiagnostic = {
    sourceLocationId: string
    retainedSource: string
    candidateObjectId: string
    distanceMeters: double
}

type CoordinateDiagnostics = {
    resolutionMethod: string
    resolvedPointCount: int
    resolvedStopCount: int
    unresolvedByCountry: CoordinateCountrySummary array
    unresolvedPointIds: string array
    unresolvedPassengerPointIds: string array
    conflictingSr70Codes: string array
    authoritativeSr70Resolutions: CoordinateResolutionDiagnostic array
    osmGapFills: CoordinateResolutionDiagnostic array
    estimatedResolutions: CoordinateResolutionDiagnostic array
    osmSr70Disagreements: CoordinateConflictDiagnostic array
    invalidSr70Identities: string array
    ambiguousOsmCandidates: string array
    corridorRejectedOsmCandidates: string array
}

type ConversionResult = {
    feed: GtfsFeed
    operationalCalls: OperationalCall array
    rejectedJourneys: RejectedJourney array
    cancelledPaIds: string array
    acceptedPaIds: string array
    sidecarBoundaryApproximations: string array
    boundaryAdjustments: BoundaryAdjustment array
    idsDiagnostics: string array
    mergeDiagnostics: string array
    coordinateDiagnostics: CoordinateDiagnostics
    notes: CzPttNote array
    features: CzPttFeature array
}

and CzPttNote = {
    id: string
    paId: string
    kind: string
    code: string option
    label: string option
    rawValue: string
    validFrom: LocalDate option
    validTo: LocalDate option
    tripIds: string array
    /// PA sequences of the note's first and last location, when resolved.
    firstSequence: int option
    lastSequence: int option
    resolved: bool
}

and CzPttFeature = {
    id: string
    tripId: string
    callSequence: int option
    sourceCode: string
    kind: string
    noteId: string option
    sourceObjectId: string
}

type internal NormalizedCall = {
    sourceIndex: int
    location: CzPttXml.CzpttLocation
    passenger: bool
    arrival: int option
    departure: int option
    lineCode: string option
    responsibleRu: string
    routeType: string
    category: string
    alternativeTransport: bool
    inferredTiming: bool
}

type internal Segment = {
    index: int
    calls: NormalizedCall array
    lineCode: string option
    responsibleRu: string
    routeType: string
    category: string
    alternativeTransport: bool
}

let emptyCatalog = {
    lines = [||]
    companies = [||]
    ids = [||]
    trainTypes = [||]
    commercialTrainTypes = [||]
    centralNotes = [||]
}

type internal Journey = {
    index: int
    modeBlockIndex: int
    calls: NormalizedCall array
    parts: Segment array
    responsibleRu: string
    routeType: string
    alternativeTransport: bool
}

let emptyPointNames: PointNameIndex = Map.empty

let internal tryString (element: JsonElement) (name: string) =
    match element.TryGetProperty(name) with
    | true, value when value.ValueKind <> JsonValueKind.Null ->
        value.GetString() |> nullOpt
    | _ -> None

let internal tryDate (element: JsonElement) (name: string) =
    tryString element name
    |> Option.bind (fun value ->
        match LocalDatePattern.Iso.Parse(value) with
        | result when result.Success -> Some result.Value
        | _ -> None)

let loadCatalogSnapshot (path: string) =
    use document = JsonDocument.Parse(IO.File.ReadAllText(path))
    let root = document.RootElement
    let array (name: string) =
        match root.TryGetProperty(name) with
        | true, value when value.ValueKind = JsonValueKind.Array ->
            value.EnumerateArray() |> Seq.toArray
        | _ -> [||]
    {
        lines =
            array "lines"
            |> Array.choose (fun value ->
                tryString value "code"
                |> Option.map (fun code -> {
                    code = code
                    mark = tryString value "mark" |> Option.defaultValue code
                    name = tryString value "name" |> Option.defaultValue ""
                    validFrom = tryDate value "valid_from"
                    validTo = tryDate value "valid_to"
                }))
        companies =
            array "companies"
            |> Array.choose (fun value ->
                tryString value "code"
                |> Option.map (fun code -> {
                    code = code
                    name = tryString value "name" |> Option.defaultValue $"Unknown {code}"
                    url = tryString value "url"
                }))
        ids =
            array "ids"
            |> Array.choose (fun value ->
                tryString value "code"
                |> Option.map (fun code -> {
                    code = code
                    abbreviation =
                        tryString value "abbreviation" |> Option.defaultValue code
                    name = tryString value "name" |> Option.defaultValue ""
                    note = tryString value "note"
                    validFrom = tryDate value "valid_from"
                    validTo = tryDate value "valid_to"
                }))
        trainTypes =
            array "train_types"
            |> Array.choose (fun value ->
                tryString value "code"
                |> Option.map (fun code -> {
                    code = code
                    abbreviation =
                        tryString value "abbreviation"
                        |> Option.defaultValue code
                }))
        commercialTrainTypes =
            array "commercial_train_types"
            |> Array.choose (fun value ->
                tryString value "code"
                |> Option.map (fun code -> {
                    code = code
                    abbreviation =
                        tryString value "abbreviation"
                        |> Option.defaultValue code
                }))
        centralNotes =
            array "central_notes"
            |> Array.choose (fun value ->
                tryString value "code"
                |> Option.map (fun code -> {
                    code = code
                    name = tryString value "name" |> Option.defaultValue code
                    text = tryString value "text"
                    validFrom = tryDate value "valid_from"
                    validTo = tryDate value "valid_to"
                }))
    }
