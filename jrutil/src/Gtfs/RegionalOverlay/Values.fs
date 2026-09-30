// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

/// Overlay value parsing, hashing and GVD date windows.
module internal JrUtil.RegionalOverlay.Values

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

let jsonOptions =
    JsonSerializerOptions(
        PropertyNamingPolicy = JsonNamingPolicy.SnakeCaseLower,
        PropertyNameCaseInsensitive = true,
        WriteIndented = true)

let requiredText name (value: string) =
    if String.IsNullOrWhiteSpace(value) then
        invalidArg name $"{name} must be non-empty"
    value

/// Capabilities a source may enable. Stop names, headsigns, trip short names,
/// calendars and agencies always stay with the base.
let capabilityNames =
    set [
        "stop_coordinates"; "boarding_points"; "call_boarding_points"; "shapes"
        "route_short_name"; "route_long_name"; "route_color"; "route_text_color"
        "transfers"; "stop_zones"; "schedules"
    ]

let sha256Tree root =
    let builder = StringBuilder()
    for path in Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories) |> Seq.sort do
        let relative = Path.GetRelativePath(root, path).Replace('\\', '/')
        let info = FileInfo(path)
        builder.Append(relative).Append('\u001f').Append(info.Length).Append('\u001f').Append(sha256File path).Append('\n') |> ignore
    sha256Text (builder.ToString())

let rowValue (row: CsvRow) name =
    match (if String.IsNullOrEmpty(name) then false, "" else row.TryGetValue(name)) with
    | true, value -> value
    | _ -> ""

let sourceIdentity fallback (row: CsvRow) =
    match rowValue row "overlay_source_id" with
    | value when String.IsNullOrWhiteSpace(value) -> fallback
    | value -> value

let originalIdentity column (row: CsvRow) =
    match rowValue row ("overlay_original_" + column) with
    | value when String.IsNullOrWhiteSpace(value) -> rowValue row column
    | value -> value

let parseInt (value: string) =
    match Int32.TryParse(value, NumberStyles.Integer, CultureInfo.InvariantCulture) with
    | true, result -> result
    | _ -> invalidOp $"Invalid integer value: {value}"

let parseDecimalOpt (value: string) =
    match Decimal.TryParse(value, NumberStyles.Float, CultureInfo.InvariantCulture) with
    | true, result -> Some result
    | _ when String.IsNullOrWhiteSpace(value) -> None
    | _ -> invalidOp $"Invalid decimal value: {value}"

let parseDate value =
    let result = LocalDatePattern.CreateWithInvariantCulture("yyyyMMdd").Parse(value)
    if not result.Success then invalidOp $"Invalid GTFS date: {value}"
    result.Value

let dateString (value: LocalDate) =
    value.ToString("yyyyMMdd", CultureInfo.InvariantCulture)

let gvdWindow year =
    let startDate, endDate = JrUtil.DateUtils.gvdBounds year
    let dates = [| for offset in 0 .. Period.Between(startDate, endDate, PeriodUnits.Days).Days -> startDate.PlusDays(offset) |]
    let index = Dictionary<LocalDate, int>()
    dates |> Array.iteri (fun i date -> index.[date] <- i)
    { startDate = startDate; endDate = endDate; dates = dates; index = index }

let emptyDates (window: DateWindow) = Array.zeroCreate<bool> window.dates.Length

let dateIntersection (left: DateSet.Dates) right = left.Intersect(right)

let anyDate (values: seq<bool>) =
    match values with | :? DateSet.Dates as dates -> dates.Any | _ -> values |> Seq.exists id

let dateKey (values: seq<bool>) =
    match values with
    | :? DateSet.Dates as dates -> dates.Key
    | _ -> (DateSet.Dates.Of values).Key
