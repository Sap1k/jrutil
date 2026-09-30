// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.Support

open System
open JrUtil.Hashing
open System.Collections.Generic
open System.Globalization
open System.IO
open System.IO.Compression
open System.Text.RegularExpressions
open System.Security.Cryptography
open System.Text
open System.Text.Json
open Microsoft.VisualBasic.FileIO
open NodaTime
open NodaTime.Text

open JrUtil.GtfsModel

open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Values
open JrUtil.RegionalOverlay.Names
open JrUtil.RegionalOverlay.GtfsFiles
open JrUtil.RegionalOverlay.StopGroups


/// The combined policy names its stop override CSV by absolute path.
let parseOverridePath path =
    if String.IsNullOrWhiteSpace(path) then None
    elif not (File.Exists(path)) then invalidOp $"Configured override CSV does not exist: {path}"
    else Some path

type OverrideBinding = {
    sourceNamespace: string
    sourceId: string
    targetNamespace: string
    targetId: string
    validFrom: LocalDate
    validTo: LocalDate
    reviewNote: string
}

let loadOverrides configuredPath =
    match parseOverridePath configuredPath with
    | None -> [||]
    | Some path ->
        csvRows (Path.GetDirectoryName(path)) (Path.GetFileName(path))
        |> Seq.map (fun row ->
            let sourceNamespace = rowValue row "source_namespace"
            let targetNamespace = rowValue row "target_namespace"
            let note = rowValue row "review_note"
            if String.IsNullOrWhiteSpace(sourceNamespace)
               || String.IsNullOrWhiteSpace(targetNamespace)
               || String.IsNullOrWhiteSpace(note) then
                invalidOp $"Override {path} must include source_namespace, target_namespace, and review_note"
            {
                sourceNamespace = sourceNamespace
                sourceId = rowValue row "source_id"
                targetNamespace = targetNamespace
                targetId = rowValue row "target_id"
                validFrom = parseDate (rowValue row "valid_from")
                validTo = parseDate (rowValue row "valid_to")
                reviewNote = note
            })
        |> Seq.toArray

let enabled policy name = policy.source.capabilities |> Array.contains name

let datesOverlap (left: DateSet.Dates) right = left.Overlaps(right)

let addDiagnostic (diagnostics: DiagnosticLog) code objectId message =
    diagnostics.Add({ code = code; sourceObjectId = objectId; message = message })

let setRow (row: CsvRow) name value =
    row.[name] <- value
    row

let cloneRow (row: CsvRow) =
    let result = CsvRow(StringComparer.Ordinal)
    for KeyValue(key, value) in row do result.[key] <- value
    result

let TransferSelectorColumns = [| "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id" |]

/// GTFS transfers carry no date validity, so source revisions that differ only
/// by date (e.g. a guaranteed wait shortened from 600 s to 300 s on 1 October)
/// can project onto one output selector tuple. Identical rows collapse. Rows
/// with the same transfer type merge conservatively: the shortest maximum
/// waiting time and the longest minimum transfer time, so no published wait is
/// longer than every source promises. Rows whose transfer types disagree have
/// no safe merge and are dropped. Returns the rows and (code, object, message)
/// diagnostics in deterministic order.
let reconcileTransferRows (rows: CsvRow seq) =
    let selector (row: CsvRow) = TransferSelectorColumns |> Array.map (rowValue row) |> String.concat "\u001f"
    let valueColumns = [| "transfer_type"; "min_transfer_time"; "max_waiting_time" |]
    let diagnostics = ResizeArray<string * string * string>()
    let reconciled =
        rows
        |> Seq.groupBy selector
        |> Seq.sortBy fst
        |> Seq.choose (fun (key, group) ->
            let group = group |> Seq.toArray
            let variants = group |> Array.distinctBy (fun row -> valueColumns |> Array.map (rowValue row))
            let objectId = key.Replace("\u001f", "|")
            if variants.Length = 1 then Some variants.[0]
            elif variants |> Array.map (fun row -> rowValue row "transfer_type") |> Array.distinct |> Array.length > 1 then
                diagnostics.Add("transfer_conflict_quarantined", objectId,
                                "Source transfer revisions disagree on transfer_type for one output selector; transfer dropped")
                None
            else
                let seconds column =
                    variants |> Array.choose (fun row ->
                        match Int32.TryParse(rowValue row column, NumberStyles.Integer, CultureInfo.InvariantCulture) with
                        | true, value -> Some value
                        | _ -> None)
                let merged = cloneRow variants.[0]
                merged.["min_transfer_time"] <-
                    match seconds "min_transfer_time" with
                    | [||] -> ""
                    | values -> string (Array.max values)
                merged.["max_waiting_time"] <-
                    match seconds "max_waiting_time" with
                    | [||] -> ""
                    | values -> string (Array.min values)
                let minimum, maximum = merged.["min_transfer_time"], merged.["max_waiting_time"]
                diagnostics.Add("transfer_date_variants_merged", objectId,
                                $"{variants.Length} source transfer revisions merged conservatively (min_transfer_time={minimum}, max_waiting_time={maximum})")
                Some merged)
        |> Seq.toArray
    reconciled, diagnostics.ToArray()

let descriptorRetrievedAtFromBase basePath =
    let manifestPath = Path.Combine(basePath, "manifest.json")
    if not (File.Exists(manifestPath)) then invalidOp "Base bundle is missing manifest.json"
    use document = JsonDocument.Parse(File.ReadAllText(manifestPath))
    let root = document.RootElement
    let mutable snapshot = Unchecked.defaultof<JsonElement>
    let mutable retrieved = Unchecked.defaultof<JsonElement>
    if root.TryGetProperty("source_snapshot", &snapshot) then ()
    else
        let mutable sources = Unchecked.defaultof<JsonElement>
        if root.TryGetProperty("sources", &sources) && sources.GetArrayLength() > 0 then
            snapshot <- sources.[0]
        else invalidOp "Base manifest does not pin source retrieval metadata"
    if not (snapshot.TryGetProperty("retrieved_at", &retrieved)) then
        invalidOp "Base manifest does not pin source retrieved_at"
    DateTimeOffset.Parse(retrieved.GetString(), CultureInfo.InvariantCulture)

let validateInputs basePath outputPath gvdYear (binding: SourceBinding) =
    if gvdYear < 2000 || gvdYear > 9999 then invalidArg "--gvd-year" "GVD year is invalid"
    if not (Directory.Exists(basePath)) then invalidArg "base-bundle" $"Base bundle does not exist: {basePath}"
    if not (File.Exists(Path.Combine(basePath, "gtfs.zip"))) then
        invalidArg "base-bundle" "Base bundle is not a production package (no gtfs.zip)"
    if Directory.Exists(outputPath) || File.Exists(outputPath) then
        invalidArg "output-bundle" "Output already exists; overlay bundles are immutable"
    if not (File.Exists(binding.payloadPath) || Directory.Exists(binding.payloadPath)) then
        invalidArg "--source" $"Source payload does not exist: {binding.payloadPath}"
    if not (File.Exists(binding.descriptorPath)) then
        invalidArg "--source-descriptor" $"Source descriptor does not exist: {binding.descriptorPath}"

/// Resolve display abbreviations only when they identify one downstream full name.
let resolveFullHeadsignDetailed (raw: string) (destinations: string array) =
    let tokens (value: string) =
        Regex.Matches(value.ToLowerInvariant(), @"[\p{L}\p{N}]+")
        |> Seq.cast<Match> |> Seq.map (fun m -> m.Value) |> Seq.toArray
    let forms (value: string) =
        [| yield tokens value
           let comma = value.IndexOf(',')
           if comma >= 0 then yield tokens value.[comma + 1..] |]
        |> Array.filter (fun value -> value.Length > 0)
        |> Array.distinct
    let abbreviatedForms = forms raw
    let tokenMatches short long =
        short = long
        || match short with
           | "žel" -> long = "železniční"
           | "st" -> long = "stanice"
           | "aut" -> long = "autobusové" || long = "autobusová"
           | "nám" -> long = "náměstí"
           | "sídl" -> long = "sídliště"
           | "zast" -> long = "zastávka"
           | _ -> false
    let matches name =
        forms name
        |> Array.exists (fun full ->
            abbreviatedForms
            |> Array.exists (fun abbreviated ->
                full.Length = abbreviated.Length
                && Array.zip abbreviated full
                   |> Array.forall (fun (short, long) -> tokenMatches short long)))
    let unique values = values |> Array.distinct
    if Regex.IsMatch(raw, @"(?i)\b(via|přes)\b|[→;]") then None, "via_or_composite"
    else
        let exact = destinations |> unique |> Array.filter (fun name -> tokens name = tokens raw)
        let locality = destinations |> unique |> Array.filter (fun name -> stopNameMatchRank raw name |> Option.isSome)
        if exact.Length = 1 then Some exact.[0], "exact_full_name"
        elif locality.Length = 1 then Some locality.[0], "locality_prefix"
        elif destinations.Length > 0 && matches destinations.[destinations.Length - 1] then
            Some destinations.[destinations.Length - 1], "terminal_name"
        else
            match destinations |> unique |> Array.filter matches with
            | [| name |] -> Some name, "known_abbreviation"
            | [||] -> None, "no_destination_match"
            | _ -> None, "ambiguous_destination"

let resolveFullHeadsign raw destinations = resolveFullHeadsignDetailed raw destinations |> fst
