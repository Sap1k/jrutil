// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023-2026 David Koňařík and contributors

/// CZPTT agencies, fare zones and feed info.
module JrUtil.CzPttAgencies

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
open JrUtil.CzPttModel
open JrUtil.CzPttNormalize
open JrUtil.CzPttEntities
open JrUtil.CzPttTransfers

let internal publicAgencyName (value: string) =
    let trimmed = value.Trim()
    let separator = trimmed.IndexOf(" - ", StringComparison.Ordinal)
    if separator > 0 then
        let baseName = trimmed.Substring(0, separator).TrimEnd()
        if String.IsNullOrWhiteSpace(baseName) then trimmed else baseName
    else trimmed

let internal agency catalog code =
    let company = catalog.companies |> Array.tryFind (fun company -> company.code = code)
    let agencyUrl =
        company
        |> Option.bind (fun value -> value.url)
        |> Option.map (fun value -> value.Trim())
        |> Option.filter (String.IsNullOrWhiteSpace >> not)
        |> Option.map (fun value ->
            if Regex.IsMatch(value, @"^[a-z][a-z0-9+.-]*://", RegexOptions.IgnoreCase)
            then value
            else "https://" + value)
        |> Option.filter (fun value ->
            match Uri.TryCreate(value, UriKind.Absolute) with
            | true, uri -> uri.Scheme = Uri.UriSchemeHttp || uri.Scheme = Uri.UriSchemeHttps
            | _ -> false)
        |> Option.defaultValue "https://portal.cisjr.cz/"
    {
        id = Some (agencyId code)
        name =
            company
            |> Option.map (fun value -> publicAgencyName value.name)
            |> Option.defaultValue $"Unknown {code}"
        url = Some agencyUrl
        timezone = "Europe/Prague"
        lang = Some "cs"
        phone = None
        fareUrl = None
        email = None
    }

let internal validIdsRecord (catalog: CatalogSnapshot) date code =
    catalog.ids
    |> Array.filter (fun value ->
        value.code = code
        && (value.validFrom |> Option.forall (fun startDate -> startDate <= date))
        && (value.validTo |> Option.forall (fun endDate -> date <= endDate)))
    |> Array.sortBy (fun value -> value.validFrom)
    |> Array.tryLast

let internal fareZone (record: CatalogIds) =
    record.note
    |> Option.bind (fun note ->
        let matched =
            Regex.Match(note, @"(?i)\bpásmo\s+(.+?)\s*$",
                        RegexOptions.CultureInvariant)
        if matched.Success then Some (matched.Groups.[1].Value.Trim())
        else None)

let internal catalogIdsSystemId (record: CatalogIds) =
    match record.abbreviation.Split('_') with
    | parts when parts.Length > 1 -> parts.[0]
    | _ -> record.abbreviation

let internal resolveIntervalEndpoint (code: string) (occurrence: int option)
                                    (calls: NormalizedCall array) =
    let normalizedCode =
        if code.Length > 2 && Char.IsLetter(code.[0]) && Char.IsLetter(code.[1])
        then code.Substring(2) else code
    let matches =
        calls
        |> Array.filter (fun call ->
            call.location.Location.LocationPrimaryCode = normalizedCode)
    match occurrence with
    | Some ordinal when ordinal > 0 && ordinal <= matches.Length ->
        Some matches.[ordinal - 1].sourceIndex
    | None when matches.Length = 1 -> Some matches.[0].sourceIndex
    | _ -> None

let internal parseOrdinal value =
    if String.IsNullOrWhiteSpace(value) then None
    else
        match Int32.TryParse(value, NumberStyles.None, CultureInfo.InvariantCulture) with
        | true, ordinal -> Some ordinal
        | _ -> Some -1

let internal feedInfo (messages: CzPttXml.CzpttcisMessage array) =
    if messages.Length = 0 then None
    else
        let starts =
            messages
            |> Array.map (fun message ->
                LocalDate.FromDateTime(
                    message.CzpttInformation.PlannedCalendar.ValidityPeriod.StartDateTime))
        let ends =
            messages
            |> Array.map (fun message ->
                message.CzpttInformation.PlannedCalendar.ValidityPeriod.EndDateTime
                |> nullableOpt
                |> Option.map LocalDate.FromDateTime
                |> Option.defaultValue (
                    LocalDate.FromDateTime(
                        message.CzpttInformation.PlannedCalendar.ValidityPeriod.StartDateTime)))
        let version =
            messages
            |> Array.maxBy (fun message -> message.CzpttCreation)
            |> fun message ->
                message.CzpttCreation.ToString(
                    "yyyyMMdd'T'HHmmss", CultureInfo.InvariantCulture)
            |> sprintf "czptt:%s"
            |> Some
        Some (
            Gtfs.obehyFeedInfo
                version
                (Some (Array.min starts))
                (Some (Array.max ends)))
