// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System
open JrUtil
open JrUtil.Hashing
open System.Collections.Generic
open System.Diagnostics
open System.Globalization
open System.IO
open System.IO.Compression
open System.Runtime
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Threading
open System.Threading.Tasks
open Parquet
open Parquet.Schema

open JrUtil.RegionalOverlay.StopGroups
open JrUtil.RegionalOverlay.Model

open JrUtil.Serving.PackageRows

/// Relations projected from an overlay's base package.
module PackageBaseRelations =
    /// A regional overlay is a semantic projection of its production base
    /// package, not a fresh generic-GTFS conversion.  Project public base
    /// semantics through the explicit base-to-output mapping so native JDF
    /// meaning survives trip slicing and replacement without depending on
    /// compiler-private tables.
    let internal projectedBaseRelations (input: CompilerOutput.Output) =
        match input.basePackage with
        | None -> Map.empty
        | Some package ->
            let targetTrips =
                CompilerOutput.values input.gtfs "trips.txt" [| "trip_id"; "service_id" |]
                |> Seq.map (fun row -> row.[0], row.[1]) |> dict
            let targetRoutes = HashSet<string>(CompilerOutput.values input.gtfs "routes.txt" [| "route_id" |] |> Seq.map (fun row -> row.[0]), StringComparer.Ordinal)
            let targetLocations = HashSet<string>(CompilerOutput.values input.gtfs "stops.txt" [| "stop_id" |] |> Seq.map (fun row -> row.[0]), StringComparer.Ordinal)
            let projections =
                CompilerOutput.values input.mappings "base_to_output_trips.csv"
                    [| "base_trip_id"; "output_trip_id" |]
                |> Seq.choose (fun row ->
                    match targetTrips.TryGetValue(row.[1]) with
                    | true, service -> Some (row.[0], struct(row.[1], service))
                    | _ -> None)
                |> Seq.groupBy fst
                |> Seq.map (fun (trip, values) ->
                    trip, values |> Seq.map snd |> Seq.distinct |> Seq.toArray)
                |> dict
            let projectTrip trip =
                match projections.TryGetValue(trip) with
                | true, targets -> targets :> seq<_>
                | _ -> Seq.empty
            let nullableText name (row: IDictionary<string,obj>) =
                match row.[name] with | null -> "" | value -> unbox<string> value
            let direct name = packageRelationRows package name
            let feed = input.feed
            let projected (idField: string) (kind: string) (identity: IDictionary<string,obj> -> string -> (string * string) list) (row: IDictionary<string,obj>) =
                let trip = nullableText "trip_id" row
                if trip = "" then
                    let route = nullableText "route_id" row
                    if route = "" || targetRoutes.Contains(route) then Seq.singleton row else Seq.empty
                else
                    projectTrip trip |> Seq.map (fun struct(target, service) ->
                        if target = trip && (isNull row.["service_id"] || unbox<string> row.["service_id"] = service) then row else
                        let result = copyRow row
                        result.["trip_id"] <- box target
                        if not (isNull result.["service_id"]) then result.["service_id"] <- box service
                        result.[idField] <- box (Identity.feedId feed kind (identity row target))
                        result)
            let notes = direct "service_note"
            let assignments =
                direct "assignment" |> Seq.collect (fun row ->
                    match unbox<string> row.["scope"] with
                    | "location" ->
                        if targetLocations.Contains(unbox<string> row.["location_id"]) then Seq.singleton row else Seq.empty
                    | _ ->
                        projected "assignment_id" "assignment-projection" (fun row target ->
                            [ "assignment", unbox<string> row.["assignment_id"]; "trip", target ]) row)
            let connections = direct "connection_claim" |> Seq.collect (fun row ->
                let origin = unbox<string> row.["origin_trip_id"]
                projectTrip origin |> Seq.map (fun struct(target, service) ->
                    if target = origin && (isNull row.["service_id"] || unbox<string> row.["service_id"] = service) then row else
                    let result = copyRow row
                    result.["origin_trip_id"] <- box target
                    if not (isNull result.["service_id"]) then result.["service_id"] <- box service
                    let originalId = unbox<string> row.["connection_id"]
                    if target <> origin then
                        result.["connection_id"] <- box (Identity.feedId feed "connection-projection" [ "connection", originalId; "trip", target ])
                    let targetTrip = nullableText "target_trip_id" row
                    if targetTrip <> "" then
                        match projectTrip targetTrip |> Seq.tryHead with
                        | Some struct(projected, _) -> result.["target_trip_id"] <- box projected
                        | None -> result.["target_trip_id"] <- null
                    result))
            let restrictions =
                direct "travel_restriction" |> Seq.collect (projected "restriction_id" "restriction-projection" (fun row target ->
                    [ "restriction", unbox<string> row.["restriction_id"]; "trip", target ]))
            Map [
                "service_note", notes
                "assignment", assignments
                "connection_claim", connections
                "travel_restriction", restrictions
            ]

    /// Deduplicate a generic relation by primary key without sorting it.
    let internal deduplicated (root: string) (relation: JrUtil.Serving.Schema.Relation) (rows: seq<IDictionary<string, obj>>) =
        let indexes = relation.fields |> Array.mapi (fun index (field: JrUtil.Serving.Schema.Field) -> field.name, index) |> dict
        let keyIndexes = relation.primaryKey |> Array.map (fun name -> indexes.[name])
        let serialized = rows |> Seq.map (fun row -> relation.fields |> Array.map (fun (field: JrUtil.Serving.Schema.Field) -> fieldText row.[field.name]))
        let size (row: string array) = 48L + int64 row.Length * 32L + (row |> Array.sumBy (fun value -> 2L * int64 value.Length))
        let key (row: string array) = keyIndexes |> Array.map (fun index -> row.[index]) |> String.concat "\u001f"
        let encode (output: BinaryWriter) (row: string array) = for value in row do output.Write(value)
        let decode (input: BinaryReader) = Array.init relation.fields.Length (fun _ -> input.ReadString())
        HashDedup.dedup root JrUtil.RegionalOverlay.Scratch.defaultBufferBytes CancellationToken.None (fun _ _ -> ())
            relation.name size key encode decode serialized
        |> Seq.map (fun row -> objectRow (Array.map2 (fun (field: JrUtil.Serving.Schema.Field) value -> field.name, parseField field value) relation.fields row))

    /// Serving mode for a basic or extended GTFS route type. Ranges follow the
    /// overlay's mode classes: coaches (200-209) are buses and urban rail
    /// (400-405) is metro.
    let servingMode routeType =
        match routeType with
        | 0 -> "tram"
        | value when value >= 900 && value <= 906 -> "tram"
        | 1 -> "metro"
        | value when value >= 400 && value <= 405 -> "metro"
        | 2 -> "rail"
        | value when value >= 100 && value <= 117 -> "rail"
        | 4 -> "water"
        | value when value >= 1000 && value <= 1021 -> "water"
        | 5 | 6 | 7 | 1400 | 1701 -> "cable"
        | value when value >= 1300 && value <= 1307 -> "cable"
        | 11 | 800 -> "trolleybus"
        | _ -> "bus"
