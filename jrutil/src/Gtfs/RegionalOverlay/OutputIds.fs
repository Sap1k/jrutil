// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

/// Output identifiers for overlay services, stops, routes and trips.
module internal JrUtil.RegionalOverlay.OutputIds

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

let serviceId dates = "overlay:service:" + (dateKey dates).Substring(0, 16)

let sourcePostId sourceId sourceStopId =
    "overlay:" + sourceId + ":post:" + (sha256Text sourceStopId).Substring(0, 16)

let sourceStopPlaceId sourceId sourceGroupId =
    "overlay:" + sourceId + ":stop-place:" + (sha256Text sourceGroupId).Substring(0, 16)

let sourceRouteOutputId sourceId sourceRouteId cisLineId =
    "overlay:" + sourceId + ":route:" + (sha256Text (sourceRouteId + "|" + cisLineId)).Substring(0, 16)

let sourceAgencyOutputId sourceId sourceAgencyId =
    "overlay:" + sourceId + ":agency:" + (sha256Text sourceAgencyId).Substring(0, 16)

let splitTripId baseTripId dates selectionFingerprint =
    let key = baseTripId + "|" + dateKey dates + "|" + selectionFingerprint
    baseTripId + ":overlay:" + (sha256Text key).Substring(0, 12)

let addedSourceTripId sourceId sourceTripId dates =
    let key = sourceId + "|" + sourceTripId + "|" + dateKey dates
    "overlay:" + sourceId + ":trip:" + (sha256Text key).Substring(0, 20)

/// Historical encoding used in split-trip IDs. Keep the exact formatting stable.
let selectionEncoding (value: OverlaySelection) =
    String.concat "|" [
        String.concat ";" value.outputStopIds
        value.outputShapeId |> Option.defaultValue ""
        value.distances
        |> Array.map (Option.map (fun item -> item.ToString(CultureInfo.InvariantCulture)) >> Option.defaultValue "")
        |> String.concat ";"
        value.arrivals |> Array.map (Option.defaultValue "") |> String.concat ";"
        value.departures |> Array.map (Option.defaultValue "") |> String.concat ";"
    ]

let selectionKey selection =
    match selection with | None -> "base" | Some value -> value.factKey
