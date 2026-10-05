// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System
open JrUtil
open System.Collections.Generic
open System.IO

open JrUtil.RegionalOverlay.Model

open JrUtil.Serving.PackageRows
open JrUtil.Serving.PackageGtfsRelations

/// Typed semantic relations: notes, assignments, connection claims and
/// travel restrictions.
module PackageSemanticRelations =
    let private assignmentRow (row: AssignmentWriter.Row) =
        let text value = if String.IsNullOrWhiteSpace(value) then null else box value
        objectRow [
            "assignment_id", box row.id; "scope", box row.scope; "kind", box row.kind
            "route_id", text row.route; "trip_id", text row.trip
            "call_sequence", box row.callSequence; "call_sequence_to", box row.callSequenceTo
            "service_id", text row.service; "location_id", text row.location; "source_code", text row.code
            "note_id", text row.note; "source_object_id", text row.sourceObject ]

    let private noteRow (id: string) kind label text validFrom validTo serviceNoteType =
        objectRow [
            "note_id", box id; "kind", box kind; "label", nullableString label; "text", nullableString text
            "valid_from", nullableParsed date validFrom; "valid_to", nullableParsed date validTo
            "service_note_type", nullableString serviceNoteType; "source_object_id", box id ]

    /// JDF semantics from compiler sidecars (the JDF bundle writes its notes
    /// and trip features natively).
    let internal semanticRelations (nativeCalls: JrUtil.Serving.Model.NativeCallArtifacts option) (input: CompilerOutput.Output) =
        let read name columns = CompilerOutput.values input.sidecars name columns
        let feed = input.feed
        let noteKind kind =
            match kind with
            | "reservation" -> "reservation_note"
            | "service_note" -> "timetable_note"
            | value -> value
        let notesRaw () =
            read "source_notice_metadata"
                [| "source_notice_id"; "notice_kind"; "gtfs_route_id"; "gtfs_trip_id"; "label"; "text"; "valid_from"; "valid_to"; "service_note_type" |]
        let notes = notesRaw () |> Seq.map (fun row -> noteRow row.[0] (noteKind row.[1]) row.[4] row.[5] row.[6] row.[7] row.[8])
        let noteLinks = notesRaw () |> Seq.filter (fun row -> row.[2] <> "" || row.[3] <> "") |> Seq.map (fun row ->
            NoteWriter.assignment feed {
                id = row.[0]; kind = row.[1]; route = (if row.[3] = "" then row.[2] else ""); trip = row.[3]
                label = ""; text = ""; validFrom = Nullable(); validTo = Nullable(); serviceNoteType = "" } |> assignmentRow)
        let features =
            read "source_trip_feature_metadata" [| "gtfs_trip_id"; "source_code"; "feature_kind"; "source_object_id" |]
            |> Seq.map (fun row ->
                assignmentRow { AssignmentWriter.empty with
                                    id = Identity.feedId feed "trip-feature" [ "trip", row.[0]; "code", row.[1]; "kind", row.[2] ]
                                    scope = "trip"; kind = row.[2]; trip = row.[0]; code = row.[1]; sourceObject = row.[3] })
        let locationFeatures =
            read "source_location_feature_metadata" [| "gtfs_stop_id"; "source_code"; "feature_kind"; "source_object_id" |]
            |> Seq.map (fun row ->
                assignmentRow { AssignmentWriter.empty with
                                    id = Identity.feedId feed "location-feature" [ "location", row.[0]; "code", row.[1]; "kind", row.[2] ]
                                    scope = "location"; kind = row.[2]; location = row.[0]; code = row.[1]; sourceObject = row.[3] })
        let transferFacts =
            read "source_transfer_metadata"
                [| "source_transfer_id"; "gtfs_trip_id"; "source_route_stop_id"; "transfer_type"; "transfer_route_id"; "transfer_stop_id"; "transfer_stop_post_id"; "transfer_end_stop_id"; "transfer_end_stop_post_id"; "wait_minutes"; "note" |]
            |> Seq.toArray
        let transferCalls =
            let wanted = transferFacts |> Seq.map (fun row -> struct(row.[1], row.[2])) |> HashSet
            let result = Dictionary<struct(string * string), int>()
            match nativeCalls with
            | Some native ->
                for struct(trip, routeStop) as key in wanted do
                    match native.transferSequences.TryGetValue(struct(trip, integer64 routeStop)) with
                    | true, sequence -> result.Add(key, sequence)
                    | _ -> ()
            | None ->
                if wanted.Count > 0 then
                    for call in read "source_call_metadata" [| "gtfs_trip_id"; "stop_sequence"; "source_route_stop_id" |] do
                        let key = struct(call.[0], call.[2])
                        if wanted.Contains(key) && not (result.ContainsKey(key)) then
                            result.Add(key, integer call.[1])
            result
        // JDF `m` (the trip waits) and `M` (it connects).
        let direction value =
            match value with
            | "m" -> "waits_for"
            | "M" -> "connects_to"
            | other -> other
        let transfers =
            transferFacts |> Seq.map (fun row ->
                let sequence = match transferCalls.TryGetValue(struct(row.[1], row.[2])) with | true, value -> value | _ -> 0
                objectRow [
                    "connection_id", box row.[0]; "direction", box (direction row.[3]); "origin_trip_id", box row.[1]; "origin_sequence", box sequence
                    "service_id", null; "target_source_route_id", nullableString row.[4]; "target_source_trip_id", null
                    "target_source_stop_id", nullableString row.[5]; "target_source_post_id", nullableString row.[6]
                    "target_source_end_stop_id", nullableString row.[7]; "target_source_end_post_id", nullableString row.[8]
                    "wait_minutes", nullableParsed integer row.[9]; "note", nullableString row.[10]; "target_public_line", null; "target_destination_text", null
                    "target_derivation", box "structured"; "resolution_status", box "unresolved"; "target_route_id", null; "target_trip_id", null; "target_location_id", null
                    "source_object_id", box row.[0] ])
        let restrictions =
            read "source_travel_restriction_metadata"
                [| "assignment_scope"; "gtfs_route_id"; "gtfs_trip_id"; "source_route_stop_id"; "group_code"; "source_route_version" |]
            |> Seq.map (fun row ->
                let routeStop = if String.IsNullOrEmpty row.[1] then null else box (Identity.routeStopKey row.[1] row.[5] row.[3])
                objectRow [
                    "restriction_id", box (Identity.feedId feed "restriction" [ "scope", row.[0]; "route", row.[1]; "trip", row.[2]; "route_stop", row.[3]; "group", row.[4] ])
                    "scope", box row.[0]; "route_id", nullableString row.[1]; "trip_id", nullableString row.[2]
                    "source_route_stop_id", box row.[3]; "route_stop_id", routeStop; "call_sequence", null; "service_id", null
                    "group_code", box row.[4]
                    "source_object_id", box (Identity.compositeKey [ row.[0]; row.[1]; row.[2]; row.[3]; row.[4] ]) ])
        Map [
            "service_note", notes
            "assignment", Seq.concat [ noteLinks; features; locationFeatures ]
            "connection_claim", transfers
            "travel_restriction", restrictions
        ]

    /// CZPTT notes and features. A note that applies between two locations
    /// links to the calls of each trip part inside that range; a note whose
    /// range could not be resolved links to every part of its path.
    let internal czpttRelations (input: CompilerOutput.Output)
                                (calls: IDictionary<string, TripCallSummary>)
                                (nonContiguous: IDictionary<string, HashSet<int>>) =
        let read name columns = CompilerOutput.values input.sidecars name columns
        let feed = input.feed
        let partsByPath =
            czpttParts input
            |> Seq.groupBy (fun pair -> let struct(pa, _) = pair.Value in pa)
            |> Seq.map (fun (pa, parts) -> pa, parts |> Seq.map _.Key |> Seq.sort |> Seq.toArray)
            |> dict
        let sequencesOf trip =
            match nonContiguous.TryGetValue(trip) with
            | true, sequences -> sequences |> Seq.sort |> Seq.toArray
            | _ ->
                match calls.TryGetValue(trip) with
                | true, summary -> [| summary.firstSequence .. summary.lastSequence |]
                | _ -> [||]
        let noteRows =
            read "source_note_metadata"
                [| "source_note_id"; "source_pa_id"; "note_kind"; "source_code"; "gtfs_trip_id"; "label"; "raw_value"; "valid_from"; "valid_to"; "from_sequence"; "to_sequence" |]
            |> Seq.toArray
        let notes =
            noteRows
            |> Seq.distinctBy (fun row -> row.[0])
            |> Seq.map (fun row -> noteRow row.[0] row.[2] row.[5] row.[6] row.[7] row.[8] row.[3])
        let link (note: string) scope (trip: string) (range: (int * int) option) =
            let first, last = match range with | Some (first, last) -> Nullable first, Nullable last | None -> Nullable(), Nullable()
            assignmentRow { AssignmentWriter.empty with
                                id = Identity.feedId feed "note-assignment" [ "note", note; "scope", scope; "trip", trip
                                                                              "from", string first; "to", string last ]
                                scope = scope; kind = "note"; trip = trip; note = note
                                callSequence = first; callSequenceTo = last }
        let noteLinks =
            noteRows |> Seq.collect (fun row ->
                let note, pa, trip = row.[0], row.[1], row.[4]
                if trip = "" then
                    match partsByPath.TryGetValue(pa) with
                    | true, parts -> parts |> Seq.map (fun part -> link note "trip" part None)
                    | _ -> Seq.empty
                elif row.[9] = "" || row.[10] = "" then Seq.singleton (link note "trip" trip None)
                else
                    let low, high = integer row.[9], integer row.[10]
                    let inside = sequencesOf trip |> Array.filter (fun sequence -> sequence >= low && sequence <= high)
                    if inside.Length = 0 then Seq.singleton (link note "trip" trip None)
                    else Seq.singleton (link note "call_range" trip (Some (inside.[0], inside.[inside.Length - 1]))))
        let features =
            read "source_feature_metadata"
                [| "source_feature_id"; "gtfs_trip_id"; "call_sequence"; "source_code"; "feature_kind"; "note_id"; "source_object_id" |]
            |> Seq.map (fun row ->
                assignmentRow { AssignmentWriter.empty with
                                    id = Identity.feedId feed "feature" [ "feature", row.[0] ]
                                    scope = (if row.[2] = "" then "trip" else "call"); kind = row.[4]; trip = row.[1]
                                    callSequence = (if row.[2] = "" then Nullable() else Nullable(integer row.[2]))
                                    code = row.[3]; note = row.[5]; sourceObject = row.[6] })
        Map [
            "service_note", notes
            "assignment", Seq.append noteLinks features
        ]
