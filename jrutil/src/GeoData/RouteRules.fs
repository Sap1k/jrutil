// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

/// Reviewed route rules kept in jrunify-ext-geodata (`routes/`). Rules select
/// CIS lines by a licence pattern and an optional agency IČO:
/// `915003` (one licence), `915001-915019` (an inclusive range of equally long
/// licences) or `915*` (a prefix; `*` alone matches every licence). The most
/// specific rule wins: a licence, then a range (narrower first), then a prefix
/// (longer first); at equal specificity a rule naming an agency wins. Equally
/// specific rules that set the same field of one route are an error.
module JrUtil.RouteRules

open System
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.RegularExpressions
open FSharp.Data

type LicencePattern =
    | Licence of string
    | LicenceRange of low: string * high: string
    | LicencePrefix of string

type RuleSelector = {
    agencyId: string option
    licence: LicencePattern
}

let parseLicencePattern (value: string) =
    let value = value.Trim()
    let digits (text: string) = text <> "" && text |> Seq.forall Char.IsAsciiDigit
    if value.EndsWith("*") then
        let prefix = value.Substring(0, value.Length - 1)
        if prefix <> "" && not (digits prefix) then invalidArg "licence" $"Invalid licence prefix: {value}"
        LicencePrefix prefix
    elif value.Contains("-") then
        match value.Split('-') with
        | [| low; high |] when digits low && digits high && low.Length = high.Length
                               && String.CompareOrdinal(low, high) <= 0 ->
            LicenceRange(low, high)
        | _ -> invalidArg "licence" $"Invalid licence range: {value}"
    elif digits value then Licence value
    else invalidArg "licence" $"Invalid licence: {value}"

let licenceMatches (licence: string) = function
    | Licence value -> licence = value
    | LicenceRange(low, high) ->
        licence.Length = low.Length
        && String.CompareOrdinal(licence, low) >= 0
        && String.CompareOrdinal(licence, high) <= 0
    | LicencePrefix prefix -> licence.StartsWith(prefix, StringComparison.Ordinal)

let selectorMatches (agencyId: string option) (licence: string) (selector: RuleSelector) =
    licenceMatches licence selector.licence
    && (selector.agencyId.IsNone || selector.agencyId = agencyId)

/// Larger is more specific.
let specificity (selector: RuleSelector) =
    let kind, narrowness =
        match selector.licence with
        | Licence _ -> 3, 0L
        | LicenceRange(low, high) -> 2, -(Int64.Parse(high) - Int64.Parse(low))
        | LicencePrefix prefix -> 1, int64 prefix.Length
    kind, narrowness, selector.agencyId.IsSome

let selectorText (selector: RuleSelector) =
    let licence =
        match selector.licence with
        | Licence value -> value
        | LicenceRange(low, high) -> $"{low}-{high}"
        | LicencePrefix prefix -> prefix + "*"
    match selector.agencyId with
    | Some agency -> $"{agency}/{licence}"
    | None -> licence

/// The value of the most specific matching rule that sets it. Equally specific
/// rules setting different values are an error; `describe` names the route.
let mostSpecific (field: string) (describe: unit -> string) (candidates: (RuleSelector * 'T option) seq) =
    let setting = candidates |> Seq.choose (fun (selector, value) -> value |> Option.map (fun v -> selector, v)) |> Seq.toArray
    if setting.Length = 0 then None else
    let best = setting |> Array.map (fst >> specificity) |> Array.max
    match setting |> Array.filter (fun (selector, _) -> specificity selector = best) with
    | [| _, value |] -> Some value
    | tied ->
        let rules = tied |> Array.map (fst >> selectorText) |> String.concat ", "
        invalidOp $"Equally specific route rules {rules} set {field} of {describe ()}"

/// The JDF agency IČO of a `jdf:agency:<ico>:<distinction>` GTFS agency ID.
let jdfAgencyIco (gtfsAgencyId: string) =
    let parsed = Regex.Match(gtfsAgencyId, "^jdf:agency:([^:]+):[0-9]+$")
    if parsed.Success then Some (Uri.UnescapeDataString(parsed.Groups.[1].Value)) else None

/// Reads a rule CSV with an exact header; returns its rows and SHA-256.
let readRuleCsv (argument: string) (header: string array) (path: string) =
    let bytes = File.ReadAllBytes(path)
    let csv = CsvFile.Parse(Encoding.UTF8.GetString(bytes), hasHeaders = true)
    if csv.Headers <> Some header then
        let expected = String.Join(",", header)
        invalidArg argument $"{Path.GetFileName(path)} must have header {expected}"
    let rows = csv.Rows |> Seq.map (fun row -> row.Columns |> Array.map (fun value -> value.Trim())) |> Seq.toArray
    rows, Convert.ToHexString(SHA256.HashData(bytes)).ToLowerInvariant()

let parseSelector (argument: string) (line: int) (agency: string) (licence: string) =
    if agency <> "" && not (agency |> Seq.forall Char.IsAsciiDigit) then
        invalidArg argument $"Line {line}: agency_id must be an IČO: {agency}"
    try
        { agencyId = (if agency = "" then None else Some agency); licence = parseLicencePattern licence }
    with :? ArgumentException as error ->
        invalidArg argument $"Line {line}: {error.Message}"

type PresentationRule = {
    selector: RuleSelector
    shortName: string option
    color: string option
    textColor: string option
    reason: string
}

type PresentationRuleSet = {
    sha256: string option
    rules: PresentationRule array
}

type RoutePresentation = {
    shortName: string option
    color: string option
    textColor: string option
}

let emptyPresentationRules = { sha256 = None; rules = [||] }

let presentationHeader =
    [| "agency_id"; "licence"; "route_short_name"; "route_color"; "route_text_color"; "reason" |]

let loadPresentationRules (path: string) =
    let argument = "routePresentationRules"
    let rows, sha256 = readRuleCsv argument presentationHeader path
    let optional (value: string) = if value = "" then None else Some value
    let color line field (value: string) =
        if value = "" then None
        elif Regex.IsMatch(value, "^[0-9A-Fa-f]{6}$") then Some (value.ToLowerInvariant())
        else invalidArg argument $"Line {line}: {field} must be six hex digits: {value}"
    let rules =
        rows
        |> Array.mapi (fun index row ->
            let line = index + 2
            let rule = {
                selector = parseSelector argument line row.[0] row.[1]
                shortName = optional row.[2]
                color = color line "route_color" row.[3]
                textColor = color line "route_text_color" row.[4]
                reason = row.[5] }
            if rule.reason = "" then invalidArg argument $"Line {line}: reason is required"
            if rule.shortName.IsNone && rule.color.IsNone && rule.textColor.IsNone then
                invalidArg argument $"Line {line}: rule overrides nothing"
            rule)
    { sha256 = Some sha256; rules = rules }

/// Applies the reviewed overrides to a computed presentation.
let resolvePresentation (ruleSet: PresentationRuleSet) (agencyId: string option) (licence: string)
                        (computed: RoutePresentation) =
    if ruleSet.rules.Length = 0 then computed else
    let matching = ruleSet.rules |> Array.filter (fun rule -> selectorMatches agencyId licence rule.selector)
    if matching.Length = 0 then computed else
    let describe () =
        match agencyId with
        | Some agency -> $"licence {licence} of agency {agency}"
        | None -> $"licence {licence}"
    let field name (value: PresentationRule -> string option) fallback =
        matching
        |> Seq.map (fun rule -> rule.selector, value rule)
        |> mostSpecific name describe
        |> Option.orElse fallback
    { shortName = field "route_short_name" _.shortName computed.shortName
      color = field "route_color" _.color computed.color
      textColor = field "route_text_color" _.textColor computed.textColor }

type PresentationChange = {
    routeId: string
    field: string
    before: string option
    after: string option
}

/// Resolves one published route and lists the fields the rules changed.
let presentRoute (ruleSet: PresentationRuleSet) (routeId: string) (agencyId: string option) (licence: string)
                 (computed: RoutePresentation) =
    let presented = resolvePresentation ruleSet agencyId licence computed
    let changes =
        [| "route_short_name", computed.shortName, presented.shortName
           "route_color", computed.color, presented.color
           "route_text_color", computed.textColor, presented.textColor |]
        |> Array.filter (fun (_, before, after) -> before <> after)
        |> Array.map (fun (field, before, after) ->
            { routeId = routeId; field = field; before = before; after = after })
    presented, changes
