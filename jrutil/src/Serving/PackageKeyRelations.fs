// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System
open JrUtil
open System.Collections.Generic
open System.Globalization
open System.IO

open JrUtil.RegionalOverlay.Model

open JrUtil.Serving.PackageRows
open JrUtil.Serving.PackageGtfsRelations

/// `source_key` and `call_key`: how source identifiers reach public ids.
/// A public id resolves to itself without a row, so identity bindings are
/// never written, and a call key exists only where the source sequence
/// differs from `trip_call.sequence`.
module PackageKeyRelations =
    /// Namespaces derived from the Czech extension tables on every build; the
    /// overlay re-derives them for its output trips instead of carrying them.
    let private nationalNamespaces = set [ "cis:line"; "cis:line_trip"; "czptt:train_number"; "czptt:pa"; "czptt:tr" ]

    /// CIS line numbers are six digits, zero-padded. Overlay routes without
    /// one carry a `source:` placeholder, which is no CIS line.
    let private cisLine (line: string) = line.PadLeft(6, '0')
    let private isCisLine (line: string) = line <> "" && line.Length <= 6 && line |> Seq.forall Char.IsAsciiDigit

    let private declared = Schema.namespaces |> Array.map _.name |> Set.ofArray

    let private entityRow kind keyNamespace (identifier: string) (publicId: string) (first: DateOnly) (last: DateOnly) (method: string) =
        objectRow [
            "entity_kind", box kind; "namespace", box keyNamespace; "identifier", box identifier
            "public_id", box publicId; "valid_from", box first; "valid_to", box last; "binding_method", box method ]

    let private callKeyRow keyNamespace (identifier: string) (sourceSequence: string) (trip: string) (sequence: int) =
        objectRow [
            "namespace", box keyNamespace; "identifier", box identifier; "source_sequence", box sourceSequence
            "trip_id", box trip; "sequence", box sequence ]

    let internal keyRelations (input: CompilerOutput.Output)
                              (targetCalls: IDictionary<string, TripCallSummary>)
                              (nonContiguousTripSequences: IDictionary<string, HashSet<int>>) =
        let bounds = serviceBounds input
        let serviceOf =
            CompilerOutput.values input.gtfs "trips.txt" [| "trip_id"; "service_id"; "route_id" |]
            |> Seq.map (fun row -> row.[0], struct(row.[1], row.[2])) |> dict
        let tripBounds trip = let struct(service, _) = serviceOf.[trip] in bounds.[service]
        let packageFirst, packageLast =
            if bounds.Count = 0 then DateOnly(1970, 1, 1), DateOnly(1970, 1, 1)
            else bounds.Values |> Seq.map fst |> Seq.min, bounds.Values |> Seq.map snd |> Seq.max
        // Route validity: the dates of the route's trips.
        let routeBounds =
            let result = Dictionary<string, DateOnly * DateOnly>(StringComparer.Ordinal)
            for KeyValue(_, struct(service, route)) in serviceOf do
                let first, last = bounds.[service]
                result.[route] <-
                    match result.TryGetValue(route) with
                    | true, (low, high) -> min low first, max high last
                    | _ -> first, last
            result
        let trip (binding: TripBinding) = tripKeyRow binding

        // 1. Regional source trips matched to output trips (overlay).
        let mappings =
            if CompilerOutput.has input.mappings "source_to_output_trips.csv" then
                CompilerOutput.values input.mappings "source_to_output_trips.csv"
                    [| "source_id"; "source_trip_id"; "output_trip_id"; "valid_from"; "valid_to"; "method" |]
                |> Seq.toArray
            else [||]
        let sourceBindings =
            mappings |> Array.choose (fun row ->
                if row.[1] = row.[2] then None else
                Some ({
                    bindingKey = ""; sourceId = row.[0]; keyNamespace = sourceNamespace row.[0] "gtfs_trip_id"
                    identifier = row.[1]; tripId = row.[2]; serviceId = (let struct(service, _) = serviceOf.[row.[2]] in service)
                    validFrom = date row.[3]; validTo = date row.[4]; method = bindingMethod row.[5] } : JrUtil.Serving.Model.TripBinding))
        let mappedTrips = HashSet<struct(string * string * string)>(mappings |> Seq.map (fun row -> struct(row.[0], row.[1], row.[2])))

        // 2. IDS JMK line/course crosswalk for the matched source trips.
        let byRange =
            mappings
            |> Seq.groupBy (fun row -> struct(row.[0], row.[1], row.[3], row.[4]))
            |> Seq.map (fun (key, rows) -> key, rows |> Seq.toArray) |> dict
        let courses =
            CompilerOutput.values input.mappings "operational_to_source_trips.csv"
                [| "source_id"; "operational_line_id"; "operational_trip_id"; "source_trip_id"; "valid_from"; "valid_to" |]
            |> Seq.collect (fun row ->
                match byRange.TryGetValue(struct(row.[0], row.[3], row.[4], row.[5])) with
                | false, _ -> Seq.empty
                | true, candidates -> candidates |> Seq.map (fun mapping ->
                    ({ bindingKey = ""; sourceId = row.[0]; keyNamespace = sourceNamespace row.[0] "line_course"
                       identifier = Identity.compositeKey [ row.[1]; row.[2] ]; tripId = mapping.[2]
                       serviceId = (let struct(service, _) = serviceOf.[mapping.[2]] in service)
                       validFrom = date row.[4]; validTo = date row.[5]; method = "operator_crosswalk" } : JrUtil.Serving.Model.TripBinding)))

        // 3. National keys from the Czech extension tables.
        let czechTrips = CompilerOutput.rows input.czech "cz_trips.txt" |> Seq.filter (fun row -> serviceOf.ContainsKey(value "trip_id" row))
        let nationalTrips = czechTrips |> Seq.collect (fun row ->
            let target = value "trip_id" row
            let first, last = tripBounds target
            seq {
                let line, number = value "cis_line_id" row, value "cis_trip_id" row
                if isCisLine line && number <> "" then
                    yield entityRow "trip" "cis:line_trip" (cisLine line + ":" + string (integer64 number)) target first last "identity"
                let train = value "train_number" row
                if train <> "" then yield entityRow "trip" "czptt:train_number" train target first last "identity"
                for token in (value "source_trip_ids" row).Split('|', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) do
                    let separator = token.IndexOf('=')
                    if separator > 0 && separator < token.Length - 1 then
                        let keyNamespace =
                            match token.Substring(0, separator) with
                            | "PA" -> "czptt:pa" | "TR" -> "czptt:tr" | _ -> null
                        if not (isNull keyNamespace) then
                            yield entityRow "trip" keyNamespace (token.Substring(separator + 1)) target first last "identity"
            })
        let nationalRoutes =
            CompilerOutput.rows input.czech "cz_routes.txt"
            |> Seq.choose (fun row ->
                let route, line = value "route_id" row, value "cis_line_id" row
                match routeBounds.TryGetValue(route) with
                | true, (first, last) when isCisLine line -> Some (entityRow "route" "cis:line" (cisLine line) route first last "identity")
                | _ -> None)

        // 4. Regional source routes and stops mapped to public ones.
        let entityMappings kind mapping sourceColumn targetColumn name =
            CompilerOutput.values input.mappings mapping [| "source_id"; sourceColumn; targetColumn |]
            |> Seq.filter (fun row -> row.[1] <> row.[2])
            |> Seq.map (fun row ->
                let method = if row.[2].StartsWith("overlay:", StringComparison.Ordinal) then "source_native" else "structural_match"
                entityRow kind (sourceNamespace row.[0] name) row.[1] row.[2] packageFirst packageLast method)
        let regionalEntities =
            Seq.append
                (entityMappings "route" "source_to_output_routes.csv" "source_route_id" "output_route_id" "gtfs_route_id")
                (entityMappings "location" "source_to_output_stops.csv" "source_stop_id" "output_stop_id" "gtfs_stop_id")

        // 5. Base package keys the overlay cannot re-derive: re-targeted onto
        // the output trips cut from each base trip, clipped to the cut's dates.
        let baseSlices =
            CompilerOutput.values input.mappings "base_to_output_trips.csv" [| "base_trip_id"; "output_trip_id"; "valid_from"; "valid_to" |]
            |> Seq.groupBy (fun row -> row.[0])
            |> Seq.map (fun (trip, rows) -> trip, rows |> Seq.toArray) |> dict
        let targetRoutes = HashSet<string>(CompilerOutput.values input.gtfs "routes.txt" [| "route_id" |] |> Seq.map (fun row -> row.[0]), StringComparer.Ordinal)
        let targetLocations = HashSet<string>(CompilerOutput.values input.gtfs "stops.txt" [| "stop_id" |] |> Seq.map (fun row -> row.[0]), StringComparer.Ordinal)
        let baseRelation name columns =
            match input.basePackage with
            | Some package when File.Exists(Path.Combine(package, "serving", name + ".parquet")) ->
                PackageReader.readTextRows (Path.Combine(package, "serving", name + ".parquet")) columns
            | _ -> Seq.empty
        let baseKeys =
            baseRelation "source_key" [| "entity_kind"; "namespace"; "identifier"; "public_id"; "valid_from"; "valid_to"; "binding_method" |]
            |> Seq.filter (fun row -> not (nationalNamespaces.Contains row.[1]))
            |> Seq.collect (fun row ->
                match row.[0] with
                | "trip" ->
                    match baseSlices.TryGetValue(row.[3]) with
                    | true, slices -> slices |> Seq.choose (fun slice ->
                        let first, last = max (date row.[4]) (date slice.[2]), min (date row.[5]) (date slice.[3])
                        if first > last || not (serviceOf.ContainsKey(slice.[1])) then None
                        else Some (entityRow "trip" row.[1] row.[2] slice.[1] first last row.[6]))
                    | _ -> Seq.empty
                | "route" when targetRoutes.Contains(row.[3]) -> Seq.singleton (entityRow "route" row.[1] row.[2] row.[3] (date row.[4]) (date row.[5]) row.[6])
                | "location" when targetLocations.Contains(row.[3]) -> Seq.singleton (entityRow "location" row.[1] row.[2] row.[3] (date row.[4]) (date row.[5]) row.[6])
                | _ -> Seq.empty)

        // Key namespaces are a closed list; a regional source without a
        // declared namespace (a minor version adds one) publishes no keys.
        let isDeclared (row: IDictionary<string, obj>) = declared.Contains(unbox<string> row.["namespace"])
        let sourceKeys =
            Seq.concat [
                sourceBindings |> Seq.map trip
                courses |> Seq.map trip
                nationalTrips; nationalRoutes; regionalEntities; baseKeys ]
            |> Seq.filter isDeclared

        // Call keys. Regional source calls whose source sequence differs.
        let regionalCalls =
            CompilerOutput.values input.mappings "source_to_output_calls.csv"
                [| "source_id"; "source_trip_id"; "output_trip_id"; "source_call_ordinal"; "output_call_ordinal" |]
            |> Seq.filter (fun row -> row.[3] <> row.[4] && mappedTrips.Contains(struct(row.[0], row.[1], row.[2])))
            |> Seq.map (fun row -> callKeyRow (sourceNamespace row.[0] "gtfs_trip_id") row.[1] row.[3] row.[2] (integer row.[4]))
        // CZPTT path sequences that differ from the trip part's.
        let pathCalls =
            CompilerOutput.values input.sidecars "source_call_metadata" [| "gtfs_trip_id"; "stop_sequence"; "source_pa_id"; "source_sequence" |]
            |> Seq.filter (fun row -> row.[2] <> "" && row.[1] <> row.[3])
            |> Seq.map (fun row -> callKeyRow "czptt:pa" row.[2] row.[3] row.[0] (integer row.[1]))
        // Base call keys follow their trip onto the output trips that still
        // hold the call.
        let holds target sequence =
            match targetCalls.TryGetValue(target) with
            | true, calls when sequence >= calls.firstSequence && sequence <= calls.lastSequence ->
                match nonContiguousTripSequences.TryGetValue(target) with
                | true, sequences -> sequences.Contains(sequence)
                | _ -> true
            | _ -> false
        let baseCalls =
            baseRelation "call_key" [| "namespace"; "identifier"; "source_sequence"; "trip_id"; "sequence" |]
            |> Seq.collect (fun row ->
                match baseSlices.TryGetValue(row.[3]) with
                | true, slices ->
                    let sequence = integer row.[4]
                    slices |> Seq.map (fun slice -> slice.[1]) |> Seq.distinct
                    |> Seq.filter (fun target -> holds target sequence)
                    |> Seq.map (fun target -> callKeyRow row.[0] row.[1] row.[2] target sequence)
                | _ -> Seq.empty)
        Map [
            "source_key", sourceKeys
            "call_key", Seq.concat [ regionalCalls; pathCalls; baseCalls ] |> Seq.filter isDeclared
        ]
