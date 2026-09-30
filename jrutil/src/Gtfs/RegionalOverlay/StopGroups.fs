// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

/// Source and base stop grouping.
module internal JrUtil.RegionalOverlay.StopGroups

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

type StopGroup = {
    groupId: string
    name: string
    lat: decimal option
    lon: decimal option
    members: CsvRow array
}

let averageCoordinate column (rows: CsvRow array) =
    let values = rows |> Array.choose (rowValue >> fun reader -> reader column |> parseDecimalOpt)
    if values.Length = 0 then None else Some (Array.average values)

let stopGroupPoints (group: StopGroup) =
    let memberPoints =
        group.members
        |> Array.choose (fun row ->
            match parseDecimalOpt (rowValue row "stop_lat"), parseDecimalOpt (rowValue row "stop_lon") with
            | Some lat, Some lon -> Some (lat, lon)
            | _ -> None)
        |> Array.distinct
    if memberPoints.Length > 0 then memberPoints
    else
        match group.lat, group.lon with
        | Some lat, Some lon -> [| lat, lon |]
        | _ -> [||]

let groupSourceStops (policy: OverlayPolicy) (scheduledStopIds: Set<string>) (rows: CsvRow array) =
    let byId = rows |> Array.map (fun row -> rowValue row "stop_id", row) |> dict
    rows
    |> Array.filter (fun row -> scheduledStopIds.Contains(rowValue row "stop_id"))
    |> Array.groupBy (fun row ->
        match optionText (rowValue row "parent_station") with
        | Some parent -> parent
        | None ->
            match optionText (rowValue row policy.source.stopMatch.groupColumn) with
            | Some group when policy.source.stopMatch.splitFlatGroupsByName ->
                "group:" + group + ":" + (normalizeName (rowValue row "stop_name") |> String.concat "-")
            | Some group -> "group:" + group
            | None -> "stop:" + rowValue row "stop_id")
    |> Array.map (fun (groupId, members) ->
        let representative =
            let explicitParent =
                members
                |> Array.tryPick (fun row -> optionText (rowValue row "parent_station"))
            match explicitParent with
            | Some parent when byId.ContainsKey(parent) -> byId.[parent]
            | _ -> members.[0]
        {
            groupId = groupId
            name = rowValue representative "stop_name"
            lat = averageCoordinate "stop_lat" members
            lon = averageCoordinate "stop_lon" members
            members = members
        })

let groupBaseStops (rows: CsvRow array) (placeByStop: IDictionary<string, string>) =
    let byId = rows |> Array.map (fun row -> rowValue row "stop_id", row) |> dict
    rows
    |> Array.groupBy (fun row ->
        let id = rowValue row "stop_id"
        match placeByStop.TryGetValue(id) with
        | true, place -> place
        | _ -> optionText (rowValue row "parent_station") |> Option.defaultValue id)
    |> Array.map (fun (groupId, members) ->
        let representative = if byId.ContainsKey(groupId) then byId.[groupId] else members.[0]
        {
            groupId = groupId
            name = rowValue representative "stop_name"
            lat = parseDecimalOpt (rowValue representative "stop_lat") |> Option.orElseWith (fun () -> averageCoordinate "stop_lat" members)
            lon = parseDecimalOpt (rowValue representative "stop_lon") |> Option.orElseWith (fun () -> averageCoordinate "stop_lon" members)
            members = members
        })
