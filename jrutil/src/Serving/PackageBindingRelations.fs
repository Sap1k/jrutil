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

open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay.Model

open JrUtil.Serving.PackageRows
open JrUtil.Serving.PackageBaseRelations
open JrUtil.Serving.PackageGtfsRelations

/// Source binding relations (source trip, call and entity maps).
module PackageBindingRelations =
    let internal bindingRows (nativeCalls: JrUtil.Serving.Model.NativeCallArtifacts option) (input: CompilerOutput.Output) (manifest: JsonElement)
                            (targetCalls: IDictionary<string, TripCallSummary>)
                            (nonContiguousTripSequences: IDictionary<string, HashSet<int>>)
                            (targetCallSchedules: IDictionary<string, TargetCallSchedule>) =
        let stringPool = Dictionary<string,string>(StringComparer.Ordinal)
        let intern (value: string) =
            if isNull value then null else
            match stringPool.TryGetValue(value) with
            | true, existing -> existing
            | _ -> stringPool.Add(value, value); value
        let targetTrips =
            CompilerOutput.values input.gtfs "trips.txt" [| "trip_id"; "service_id"; "direction_id"; "block_id" |]
            |> Seq.map (fun row -> intern row.[0], struct(intern row.[1], intern row.[2], intern row.[3])) |> dict
        let mappings =
            let rows =
                if CompilerOutput.has input.mappings "source_to_output_trips.csv" then
                    CompilerOutput.values input.mappings "source_to_output_trips.csv"
                        [| "source_id"; "source_trip_id"; "output_trip_id"; "valid_from"; "valid_to"; "method" |]
                else Seq.empty
            rows
            |> Seq.map (fun row -> {
                sourceId = intern row.[0]; sourceTripId = intern row.[1]; outputTripId = intern row.[2]
                validFrom = intern row.[3]; validTo = intern row.[4]; methodName = intern row.[5] })
            |> Seq.toArray
        let mappingsBySourceRange =
            mappings
            |> Seq.groupBy (fun row ->
                struct(row.sourceId, row.sourceTripId, row.validFrom, row.validTo))
            |> Seq.map (fun (key, rows) -> key, rows |> Seq.toArray)
            |> dict
        let bounds = serviceBounds input
        let snapshots = combinedSourceManifest input manifest
        let defaultSource = defaultSourceId manifest snapshots
        let binding sourceId sourceTrip target first last variant : TripBinding =
                let sourceId, sourceTrip, target = intern sourceId, intern sourceTrip, intern target
                let first, last, variant = intern first, intern last, intern variant
                let struct(service, direction, block) = targetTrips.[target]
                let calls = targetCalls.[target]
                let fields = [ "source_id", sourceId; "namespace", "gtfs_trip_id"; "source_trip_id", sourceTrip; "trip_id", target; "service_id", service; "valid_from", first; "valid_to", last ]
                let bindingId = Identity.bindingId "trip" fields
                { binding_id = bindingId; source_id = sourceId; trip_namespace = "gtfs_trip_id"; source_trip_id = sourceTrip
                  trip_id = target; service_id = service; valid_from = date first; valid_to = date last
                  binding_status = "confirmed"; scheduled_start = calls.scheduledStart; scheduled_end = calls.scheduledEnd
                  source_route_id = null; source_direction_id = direction; source_start_location_id = intern calls.firstStopId
                  source_end_location_id = intern calls.lastStopId; source_block_id = block
                  call_pattern_sha256 = intern calls.callPatternSha256; variant_key = variant }
        let compilerBindings =
            if mappings.Length > 0 then
                mappings |> Seq.map (fun row ->
                    binding row.sourceId row.sourceTripId row.outputTripId row.validFrom row.validTo row.methodName)
            else
                CompilerOutput.rows input.gtfs "trips.txt" |> Seq.map (fun row ->
                    let trip, service = value "trip_id" row, value "service_id" row
                    let first, last = bounds.[service]
                    binding defaultSource trip trip (first.ToString("yyyyMMdd")) (last.ToString("yyyyMMdd")) "source_native")
            |> Seq.toArray
        let basePackage = input.basePackage
        let baseSlices =
            let rows =
                if CompilerOutput.has input.mappings "base_to_output_trips.csv" then
                    CompilerOutput.values input.mappings "base_to_output_trips.csv"
                        [| "base_trip_id"; "output_trip_id"; "valid_from"; "valid_to" |]
                else Seq.empty
            rows
            |> Seq.map (fun row -> {
                baseTripId = row.[0]; outputTripId = row.[1]; validFrom = row.[2]; validTo = row.[3] })
            |> Seq.groupBy _.baseTripId
            |> Seq.map (fun (trip, rows) -> trip, rows |> Seq.toArray)
            |> dict
        let baseBindingPairs =
            match basePackage with
            | None -> [||]
            | Some package when not (File.Exists(Path.Combine(package, "serving", "source_trip_map.parquet"))) -> [||]
            | Some package ->
                PackageReader.readTextRows (Path.Combine(package, "serving", "source_trip_map.parquet"))
                    [| "binding_id"; "source_id"; "trip_namespace"; "source_trip_id"; "trip_id"; "valid_from"; "valid_to"; "binding_status"; "source_route_id"; "source_direction_id"; "source_start_location_id"; "source_end_location_id"; "source_block_id"; "variant_key" |]
                |> Seq.collect (fun row ->
                    match baseSlices.TryGetValue(row.[4]) with
                    | false, _ -> Seq.empty
                    | true, slices -> slices |> Seq.choose (fun slice ->
                        let first = max (date row.[5]) (date slice.validFrom)
                        let last = min (date row.[6]) (date slice.validTo)
                        if first > last then None else
                        let target = slice.outputTripId
                        let result = binding row.[1] row.[3] target (first.ToString("yyyyMMdd")) (last.ToString("yyyyMMdd")) row.[13]
                        let fields = [ "source_id", row.[1]; "namespace", row.[2]; "source_trip_id", row.[3]; "trip_id", target; "service_id", result.service_id; "valid_from", first.ToString("yyyyMMdd"); "valid_to", last.ToString("yyyyMMdd") ]
                        let result = { result with
                                        trip_namespace = row.[2]; binding_status = row.[7]
                                        source_route_id = row.[8]; source_direction_id = row.[9]
                                        source_start_location_id = row.[10]; source_end_location_id = row.[11]
                                        source_block_id = row.[12]
                                        binding_id = Identity.bindingId "trip" fields }
                        Some (row.[0], result)))
                |> Seq.toArray
        let primaryBindings =
            Seq.append compilerBindings (baseBindingPairs |> Seq.map snd)
            |> Seq.distinctBy (fun row -> row.binding_id)
            |> Seq.toArray
        let primaryByTarget = lazy (
            let result = Dictionary<string, TripBinding>(StringComparer.Ordinal)
            for row in primaryBindings do result.TryAdd(row.trip_id, row) |> ignore
            result)
        let czpttBindings =
            CompilerOutput.rows input.czech "cz_trips.txt"
            |> Seq.collect (fun row ->
                let target = value "trip_id" row
                match targetTrips.TryGetValue(target) with
                | false, _ -> Seq.empty
                | true, struct(service, _, _) ->
                    let first, last = bounds.[service]
                    (value "source_trip_ids" row).Split(
                        '|', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)
                    |> Seq.choose (fun token ->
                        let separator = token.IndexOf('=')
                        if separator <= 0 || separator = token.Length - 1 then None else
                        let prefix, identifier = token.Substring(0, separator), token.Substring(separator + 1)
                        let namespaceName = if prefix = "PA" then "czptt_pa_id" elif prefix = "TR" then "czptt_tr_id" else ""
                        if namespaceName = "" then None else
                        let fields = [ "source_id", defaultSource; "namespace", namespaceName; "source_trip_id", identifier; "trip_id", target; "service_id", service; "valid_from", first.ToString("yyyyMMdd"); "valid_to", last.ToString("yyyyMMdd") ]
                        let context = primaryByTarget.Value.[target]
                        Some { context with
                                binding_id = Identity.bindingId "trip" fields; source_id = defaultSource
                                trip_namespace = namespaceName; source_trip_id = identifier; trip_id = target; service_id = service
                                valid_from = first; valid_to = last; binding_status = "confirmed"
                                source_route_id = null; variant_key = "czptt-source-identity-v1" }))
        let bindings = Seq.append primaryBindings czpttBindings |> Seq.toArray
        let bindingByMapping = lazy (
            bindings |> Seq.map (fun row ->
                let key = row.source_id, row.source_trip_id, row.trip_id
                key, row) |> Seq.groupBy fst |> Seq.map (fun (key, values) -> key, values |> Seq.map snd |> Seq.toArray) |> dict)
        let compilerCalls =
            if nativeCalls.IsSome then Seq.empty
            elif mappings.Length > 0 then
                CompilerOutput.values input.mappings "source_to_output_calls.csv"
                    [| "source_id"; "source_trip_id"; "output_trip_id"; "source_call_ordinal"; "output_call_ordinal"; "source_stop_id" |]
                |> Seq.collect (fun row ->
                    let key = row.[0], row.[1], row.[2]
                    match bindingByMapping.Value.TryGetValue(key) with
                    | false, _ -> Seq.empty
                    | true, matching ->
                        let sequence = integer row.[4]
                        let schedule = targetCallSchedules.[row.[2]]
                        let offset = sequence - schedule.firstSequence
                        let direct =
                            if offset >= 0 && offset < schedule.calls.Length then Some schedule.calls.[offset]
                            else None
                        let binaryFind () =
                            let mutable low, high = 0, schedule.calls.Length - 1
                            let mutable result = None
                            while result.IsNone && low <= high do
                                let middle = low + (high - low) / 2
                                let struct(candidate, _, _) as call = schedule.calls.[middle]
                                if candidate = sequence then result <- Some call
                                elif candidate < sequence then low <- middle + 1
                                else high <- middle - 1
                            result
                        let matched =
                            match direct with
                            | Some (struct(candidate, _, _) as call) when candidate = sequence -> Some call
                            | _ -> binaryFind ()
                        let arrival, departure =
                            match matched with
                            | Some struct(_, arrival, departure) -> arrival, departure
                            | None -> Nullable(), Nullable()
                        matching |> Seq.map (fun binding ->
                            ({ binding = binding.binding_id;
                              callNamespace = "gtfs_stop_sequence"; sourceSequence = row.[3]; sequence = sequence;
                              stop = row.[5]; arrival = arrival; departure = departure } : SourceCallWriter.MappedRow)))
            else
                let bindingByTrip = bindings |> Seq.map (fun row -> row.trip_id, row) |> dict
                CompilerOutput.rows input.gtfs "stop_times.txt" |> Seq.map (fun row ->
                    let sequence = value "stop_sequence" row
                    let binding = bindingByTrip.[value "trip_id" row]
                    let optional value = let parsed = seconds value in if isNull parsed then Nullable() else Nullable(unbox<int> parsed)
                    ({ binding = binding.binding_id;
                      callNamespace = "gtfs_stop_sequence"; sourceSequence = sequence; sequence = integer sequence;
                      stop = value "stop_id" row; arrival = optional (value "arrival_time" row);
                      departure = optional (value "departure_time" row) } : SourceCallWriter.MappedRow))
        let baseBindingReplacements =
            baseBindingPairs |> Seq.groupBy fst |> Seq.map (fun (key, values) -> key, values |> Seq.map snd |> Seq.toArray) |> dict
        let baseCalls =
            match basePackage with
            | None -> Seq.empty
            | Some package when not (File.Exists(Path.Combine(package, "serving", "source_call_map.parquet"))) -> Seq.empty
            | Some package ->
                PackageReader.readTextRows (Path.Combine(package, "serving", "source_call_map.parquet"))
                    [| "binding_id"; "call_namespace"; "source_sequence"; "call_sequence"; "source_stop_id"; "scheduled_arrival"; "scheduled_departure" |]
                |> Seq.collect (fun row ->
                    match baseBindingReplacements.TryGetValue(row.[0]) with
                    | false, _ -> Seq.empty
                    | true, targetBindings -> targetBindings |> Seq.choose (fun targetBinding ->
                        let target = targetBinding.trip_id
                        let sequence = integer row.[3]
                        match targetCalls.TryGetValue(target) with
                        | false, _ -> None
                        | true, targetRows
                            when sequence >= targetRows.firstSequence && sequence <= targetRows.lastSequence
                                 && (match nonContiguousTripSequences.TryGetValue(target) with
                                     | true, sequences -> sequences.Contains(sequence)
                                     | _ -> true) ->
                            let optional value = if String.IsNullOrWhiteSpace(value) then Nullable() else Nullable(integer value)
                            Some ({ binding = targetBinding.binding_id; callNamespace = row.[1];
                                   sourceSequence = row.[2]; sequence = sequence; stop = row.[4];
                                   arrival = optional row.[5]; departure = optional row.[6] } : SourceCallWriter.MappedRow)
                        | _ -> None))
        let baseCoverage =
            match basePackage with
            | None -> Seq.empty
            | Some package when not (File.Exists(Path.Combine(package, "serving", "source_trip_coverage.parquet"))) -> Seq.empty
            | Some package ->
                PackageReader.readTextRows (Path.Combine(package, "serving", "source_trip_coverage.parquet"))
                    [| "binding_id"; "coverage_id"; "from_sequence"; "to_sequence"; "coverage_type"; "system_id"; "coverage_role" |]
                |> Seq.collect (fun row ->
                    match baseBindingReplacements.TryGetValue(row.[0]) with
                    | false, _ -> Seq.empty
                    | true, targetBindings -> targetBindings |> Seq.map (fun targetBinding -> objectRow [
                        "binding_id", box targetBinding.binding_id; "coverage_id", box row.[1]
                        "service_id", box targetBinding.service_id; "from_sequence", box (integer row.[2]); "to_sequence", box (integer row.[3])
                        "coverage_type", box row.[4]; "system_id", nullableString row.[5]; "coverage_role", nullableString row.[6] ]))
        let calls = Seq.append compilerCalls baseCalls
        let operational = CompilerOutput.rows input.mappings "operational_to_source_trips.csv" |> Seq.collect (fun row ->
            let key =
                struct(value "source_id" row, value "source_trip_id" row,
                       value "valid_from" row, value "valid_to" row)
            match mappingsBySourceRange.TryGetValue(key) with
            | false, _ -> Seq.empty
            | true, candidates -> candidates |> Seq.map (fun mapping ->
                    let sourceId, target = value "source_id" row, mapping.outputTripId
                    let sourceTrip = Identity.compositeKey [ value "operational_line_id" row; value "operational_trip_id" row ]
                    let struct(service, _, _) = targetTrips.[target]
                    let fields = [ "source_id", sourceId; "namespace", "operational_line_course"; "source_trip_id", sourceTrip; "trip_id", target; "service_id", service; "valid_from", value "valid_from" row; "valid_to", value "valid_to" row ]
                    let context = primaryByTarget.Value.[target]
                    { context with
                        binding_id = Identity.bindingId "trip" fields; source_id = sourceId
                        trip_namespace = "operational_line_course"; source_trip_id = sourceTrip; trip_id = target; service_id = service
                        valid_from = date (value "valid_from" row); valid_to = date (value "valid_to" row)
                        binding_status = "confirmed"; source_route_id = null
                        variant_key = "ids-jmk-api-v1" }) )
        Seq.append bindings operational, calls, baseCoverage
