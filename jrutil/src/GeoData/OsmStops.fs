// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// Public transport stops from OSM for JDF stop matching.
module JrUtil.GeoData.OsmStops

#nowarn "9"
open System
open JrUtil
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.IO.MemoryMappedFiles
open System.Runtime
open System.Runtime.CompilerServices
open System.Text
open System.Threading
open System.Threading.Tasks
open OsmSharp
open OsmSharp.Geo
open OsmSharp.Streams
open NetTopologySuite.Features
open NetTopologySuite.Geometries
open Serilog
open JrUtil.GeoData.CzRegions
open JrUtil.GeoData.Common
open JrUtil.GeoData.StopMatcher
open JrUtil.JdfModel
open JrUtil.JdfFixups
open JrUtil.Utils
open Microsoft.FSharp.NativeInterop

type CzOtherStop = {
    id: int64
    name: string option
    officialName: string option
    point: Point
    rawTags: string
    explicitModes: string
    deniedModes: string
    lifecycle: string
}

let featTagOpt name (feat: IFeature) =
    feat.Attributes.GetOptionalValue(name)
    |> nullOpt
    |> Option.map unbox<string>

let czOtherStopNameRegion =
    // Used for synonym matching
    let matcher = new StopMatcher<_>([||])
    fun (stop: CzOtherStop) etrs89ExPt ->
        // Stops within cities often omit the city's name on the pole, so we
        // have to add it back in. We assume official_name to be the full name.
        match stop.officialName,
              stop.name,
              czechTownByPoint () etrs89ExPt with
        | None, None, _ -> "", None
        | Some n, _, None -> n, None
        | Some n, _, Some (_, r, _) -> n, Some r
        | None, Some n, None -> n, None
        | None, Some n, Some (tn, r, _) ->
            if matcher.nameSimilarity(
                stopNameToTokens n, stopNameToTokens tn) = 1f
            then n, Some r
            else tn + "," + n, Some r

let getCzOtherStops pbfPath =
    use stream = File.OpenRead(pbfPath)
    (new PBFOsmStreamSource(stream)
     |> Seq.filter (fun node ->
         node.Type = OsmGeoType.Node
         && not (node.Tags.ContainsKey("train"))
         && (not (node.Tags.ContainsKey("railway"))
             || node.Tags.ContainsKey("tram"))
         && (node.Tags.Contains("highway", "bus_stop")
             || node.Tags.Contains("public_transport", "platform")
             || node.Tags.Contains("public_transport", "pole")
             || node.Tags.Contains("public_transport", "station")
             || node.Tags.Contains("railway", "tram_stop")
             || node.Tags.Contains("amenity", "bus_station"))))
     .ToFeatureSource()
    |> Seq.map (fun feat ->
        let tag name = featTagOpt name feat
        let valueIs (expected: string) = function
            | Some (value: string) -> String.Equals(value, expected, StringComparison.OrdinalIgnoreCase)
            | None -> false
        let affirmative = function
            | Some (value: string) ->
                match value.Trim().ToLowerInvariant() with
                | "yes" | "designated" | "official" -> true
                | _ -> false
            | None -> false
        let negative = function
            | Some (value: string) -> String.Equals(value.Trim(), "no", StringComparison.OrdinalIgnoreCase)
            | None -> false
        let bus = tag "bus"
        let psv = tag "psv"
        let tram = tag "tram"
        let highway = tag "highway"
        let railway = tag "railway"
        let amenity = tag "amenity"
        let explicitModes =
            [ if valueIs "bus_stop" highway || valueIs "bus_station" amenity
                 || affirmative bus || affirmative psv then
                  "road"
              if valueIs "tram_stop" railway || affirmative tram then
                  "tram" ]
            |> String.concat ";"
        let deniedModes =
            [ if negative bus || negative psv then
                  "road"
              if negative tram then
                  "tram" ]
            |> String.concat ";"
        let lifecycle =
            [ "abandoned"; "disused"; "construction"; "proposed"; "temporary" ]
            |> List.tryFind (fun key ->
                feat.Attributes.Exists(key)
                || (tag "lifecycle" |> valueIs key))
            |> Option.defaultValue "active"
        let auditKeys = [|
            "highway"; "public_transport"; "railway"; "amenity"
            "bus"; "psv"; "tram"; "trolleybus"
            "access"; "vehicle"; "motor_vehicle"
            "abandoned"; "disused"; "construction"; "proposed"; "temporary"
            // Stand number: separates numbered bays for the learned post scorer.
            "local_ref"
        |]
        let rawTags =
            auditKeys
            |> Array.choose (fun key ->
                // Values are joined with ';', so a ';' inside a value would split it.
                tag key |> Option.map (fun value -> $"""{key}={value.Replace(";", ",")}"""))
            |> String.concat ";"
        {
        id = feat.Attributes.["id"] :?> int64
        name =
            featTagOpt "name" feat
            |> Option.orElse (featTagOpt "name:cs" feat)
        officialName = featTagOpt "official_name" feat
        point = wgs84Factory.CreateGeometry(feat.Geometry) :?> Point
        rawTags = rawTags
        explicitModes = explicitModes
        deniedModes = deniedModes
        lifecycle = lifecycle
    })
    |> Seq.toArray

let czOtherStopsForJdfMatch (stops: CzOtherStop seq) =
    stops
    |> Seq.map (fun s ->
        let etrs89ExPt = pointWgs84ToEtrs89Ex s.point
        let name, region = czOtherStopNameRegion s etrs89ExPt
        {
            name = name
            data = {
                country = if region.IsSome then Some "CZ" else None
                regionId = region
                point = etrs89ExPt
                source = Some $"osm:node:{s.id}"
                candidateObservation = Some {
                    observationId = $"osm:node:{s.id}"
                    sourceKind = "osm"
                    sourceObjectId = Some $"osm:node:{s.id}"
                    observedAt = None
                    rawTags = s.rawTags
                    explicitModes = s.explicitModes
                    deniedModes = s.deniedModes
                    lifecycle = s.lifecycle
                    supportWeight = 1M
                }
            }
        })
    |> Seq.toArray
