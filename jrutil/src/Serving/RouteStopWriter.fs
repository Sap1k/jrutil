// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.Collections.Generic
open System.IO
open System.Threading

/// Package post-pass over the finalized calls of every producer: derive the
/// ordered `route_stop` slots of each route direction, point
/// `trip_call.route_stop_id` and travel restrictions at them, and write the
/// zones: on the slot when all its calls agree, otherwise on the call.
module RouteStopWriter =
    [<Struct>]
    type Zone = { code: string; system: string }

    /// Zone facts in increasing order of precedence-free fallback: an explicit
    /// call zone wins, then the zones of the call's source route stop, then the
    /// zones of its stop place.
    type ZoneInputs = {
        /// (trip_id, sequence) -> zones in source order.
        calls: IDictionary<struct(string * int), Zone array>
        /// Compiler route-stop key found in the incoming trip_call.route_stop_id.
        routeStops: IDictionary<string, Zone array>
        /// location_id -> zones.
        places: IDictionary<string, Zone array>
    }

    let noZones = {
        calls = Dictionary<struct(string * int), Zone array>()
        routeStops = Dictionary<string, Zone array>(StringComparer.Ordinal)
        places = Dictionary<string, Zone array>(StringComparer.Ordinal) }

    type Summary = {
        routeDirections: int
        patterns: int
        slots: int
        maximumPatterns: int
        routeStopZones: int
        callZones: int
        locationZones: int
        unmatchedCallZones: int
    }

    type Result = { counts: IDictionary<string, int>; summary: Summary }

    [<Struct>]
    type private Call = { sequence: int; location: int; time: int }

    type private PatternEntry(group: int, key: string, sequence: int array, first: string) =
        member _.group = group
        member _.key = key
        member _.sequence = sequence
        member val weight = 1 with get, set
        member val first = first with get, set

    type private Slot = { route: string; direction: Nullable<int16>; sequence: int; location: string; id: string }

    let private relation name = Schema.relations |> Array.find (fun value -> value.name = name)
    let private rowGroupRows = 8192

    let private replace (path: string) (write: string -> int) =
        let temporary = path + ".rewrite"
        if File.Exists(temporary) then File.Delete(temporary)
        let count = write temporary
        File.Move(temporary, path, true)
        count

    let finalize (serving: string) (zones: ZoneInputs) (progress: string -> int64 -> unit) =
        let token = CancellationToken.None
        let tripPath, callPath = Path.Combine(serving, "trip.parquet"), Path.Combine(serving, "trip_call.parquet")
        let tripSchema, callSchema = relation "trip", relation "trip_call"
        let field (schema: Schema.Relation) name = schema.fields |> Array.findIndex (fun value -> value.name = name)

        // 1. Trips grouped by route direction.
        let groupOf = Dictionary<string, int>(StringComparer.Ordinal)
        let groups = ResizeArray<struct(string * Nullable<int16>)>()
        let groupIndex = Dictionary<struct(string * Nullable<int16>), int>()
        for columns in ColumnReader.groups tripPath tripSchema do
            let trips = ColumnReader.text columns.[field tripSchema "trip_id"]
            let routes = ColumnReader.text columns.[field tripSchema "route_id"]
            let directions = ColumnReader.optionalInt16 columns.[field tripSchema "direction"]
            for row in 0 .. trips.Length - 1 do
                let key = struct(routes.[row], directions.[row])
                let index =
                    match groupIndex.TryGetValue(key) with
                    | true, index -> index
                    | _ ->
                        groupIndex.Add(key, groups.Count)
                        groups.Add(key)
                        groups.Count - 1
                groupOf.Add(trips.[row], index)

        // 2. Calls per trip, with locations interned.
        let locationIndex = Dictionary<string, int>(StringComparer.Ordinal)
        let locations = ResizeArray<string>()
        let intern location =
            match locationIndex.TryGetValue(location) with
            | true, index -> index
            | _ ->
                locationIndex.Add(location, locations.Count)
                locations.Add(location)
                locations.Count - 1
        let callsByTrip = Dictionary<string, ResizeArray<Call>>(StringComparer.Ordinal)
        let mutable read = 0L
        for columns in ColumnReader.groups callPath callSchema do
            let trips = ColumnReader.text columns.[field callSchema "trip_id"]
            let sequences = ColumnReader.int32 columns.[field callSchema "sequence"]
            let places = ColumnReader.text columns.[field callSchema "location_id"]
            let arrivals = ColumnReader.optionalInt32 columns.[field callSchema "scheduled_arrival"]
            let departures = ColumnReader.optionalInt32 columns.[field callSchema "scheduled_departure"]
            for row in 0 .. trips.Length - 1 do
                let calls =
                    match callsByTrip.TryGetValue(trips.[row]) with
                    | true, calls -> calls
                    | _ ->
                        let calls = ResizeArray<Call>()
                        callsByTrip.Add(trips.[row], calls)
                        calls
                let time =
                    if departures.[row].HasValue then departures.[row].Value
                    elif arrivals.[row].HasValue then arrivals.[row].Value
                    else Int32.MinValue
                calls.Add({ sequence = sequences.[row]; location = intern places.[row]; time = time })
            read <- read + int64 trips.Length
            progress "route-stops-read-calls" read

        // 3. Distinct patterns per route direction. Times come from the
        // ordinally smallest trip running the pattern, so input order never
        // changes the result.
        let patternIndex = Array.init groups.Count (fun _ -> Dictionary<string, int>(StringComparer.Ordinal))
        let patterns = ResizeArray<PatternEntry>()
        let tripPattern = Dictionary<string, struct(int * int array)>(StringComparer.Ordinal)
        for KeyValue(trip, calls) in callsByTrip do
            calls.Sort(fun left right -> compare left.sequence right.sequence)
            match groupOf.TryGetValue(trip) with
            | false, _ -> invalidOp $"trip_call references unknown trip {trip}"
            | true, group ->
                let sequence = calls |> Seq.map (fun call -> call.location) |> Seq.toArray
                let key = sequence |> Array.map (fun location -> locations.[location]) |> Identity.compositeKey
                let index =
                    match patternIndex.[group].TryGetValue(key) with
                    | true, index ->
                        let entry = patterns.[index]
                        entry.weight <- entry.weight + 1
                        if String.CompareOrdinal(trip, entry.first) < 0 then entry.first <- trip
                        index
                    | _ ->
                        patternIndex.[group].Add(key, patterns.Count)
                        patterns.Add(PatternEntry(group, key, sequence, trip))
                        patterns.Count - 1
                tripPattern.Add(trip, struct(index, calls |> Seq.map (fun call -> call.sequence) |> Seq.toArray))
        let fraction (calls: ResizeArray<Call>) =
            let known = calls |> Seq.filter (fun call -> call.time <> Int32.MinValue) |> Seq.toArray
            if known.Length < 2 then Array.create calls.Count Double.NaN else
            let first, last = known |> Array.minBy _.time |> _.time, known |> Array.maxBy _.time |> _.time
            calls |> Seq.map (fun call ->
                if call.time = Int32.MinValue || last <= first then Double.NaN
                else float (call.time - first) / float (last - first)) |> Seq.toArray

        // 4. Merge each direction into slots.
        let slots = ResizeArray<Slot>()
        let slotsByPattern = Array.zeroCreate<int array> patterns.Count
        let mutable patternCount, maximumPatterns = 0, 0
        let groupOrder =
            [| 0 .. groups.Count - 1 |]
            |> Array.sortWith (fun left right ->
                let struct(leftRoute, leftDirection) = groups.[left]
                let struct(rightRoute, rightDirection) = groups.[right]
                let byRoute = String.CompareOrdinal(leftRoute, rightRoute)
                if byRoute <> 0 then byRoute
                else compare (if leftDirection.HasValue then int leftDirection.Value else -1)
                             (if rightDirection.HasValue then int rightDirection.Value else -1))
        for group in groupOrder do
            let struct(route, direction) = groups.[group]
            let entries =
                patternIndex.[group].Values |> Seq.map (fun index -> patterns.[index])
                |> Seq.sortWith (fun left right -> String.CompareOrdinal(left.key, right.key)) |> Seq.toArray
            let input =
                entries |> Array.map (fun entry ->
                    { RouteStopOrder.locations = entry.sequence; RouteStopOrder.weight = entry.weight
                      RouteStopOrder.times = fraction callsByTrip.[entry.first]; RouteStopOrder.key = entry.key })
            patternCount <- patternCount + input.Length
            maximumPatterns <- max maximumPatterns input.Length
            let ordered, assigned = RouteStopOrder.merge input
            let directionText = if direction.HasValue then string direction.Value else "-"
            let occurrences = Dictionary<int, int>()
            let firstSlot = slots.Count
            ordered |> Array.iteri (fun position location ->
                let occurrence = (match occurrences.TryGetValue(location) with | true, value -> value | _ -> 0) + 1
                occurrences.[location] <- occurrence
                slots.Add({
                    route = route; direction = direction; sequence = position + 1; location = locations.[location]
                    id = Identity.compositeKey [ route; directionText; locations.[location]; string occurrence ] }))
            entries |> Array.iteri (fun index entry ->
                slotsByPattern.[patternIndex.[group].[entry.key]] <- assigned.[index] |> Array.map (fun slot -> firstSlot + slot))
        callsByTrip.Clear()

        let slotOf trip sequence =
            match tripPattern.TryGetValue(trip) with
            | true, struct(pattern, sequences) ->
                match Array.BinarySearch(sequences, sequence) with
                | index when index >= 0 -> Some slotsByPattern.[pattern].[index]
                | _ -> None
            | _ -> None

        // 5. route_stop.
        let counts = Dictionary<string, int>(StringComparer.Ordinal)
        let routeStopSchema = relation "route_stop"
        counts.["route_stop"] <-
            replace (Path.Combine(serving, "route_stop.parquet")) (fun target ->
                RelationWriter.writeRows target routeStopSchema (4L * 1024L * 1024L) rowGroupRows token progress
                    (fun (slot: Slot) -> 160L + 2L * int64 (slot.route.Length + slot.location.Length + slot.id.Length))
                    (fun rows ->
                        let column f = Array.map f rows
                        [| ColumnWriter.Text(column _.route); ColumnWriter.Text(column _.id)
                           ColumnWriter.OptionalInt16(column _.direction); ColumnWriter.Int32(column _.sequence)
                           ColumnWriter.Text(column _.location) |])
                    slots)

        // 6. Restriction rows that name a compiler route-stop key.
        let restrictionPath = Path.Combine(serving, "travel_restriction_assignment.parquet")
        let restrictionSchema = relation "travel_restriction_assignment"
        let restrictionGroups = ColumnReader.groups restrictionPath restrictionSchema |> Seq.toArray
        let wantedKeys = HashSet<string>(StringComparer.Ordinal)
        for columns in restrictionGroups do
            for key in ColumnReader.text columns.[field restrictionSchema "route_stop_id"] do
                if not (isNull key) then wantedKeys.Add(key) |> ignore
        let slotsByKey = Dictionary<string, SortedSet<int>>(StringComparer.Ordinal)
        let callByTripKey = Dictionary<struct(string * string), struct(int * int)>()
        let slotIds = HashSet<string>(slots |> Seq.map _.id, StringComparer.Ordinal)

        // 7. Rewrite trip_call and resolve each call's zones. A slot keeps
        // zones only when every call at it has the same non-empty set.
        let zonedCalls = ResizeArray<struct(string * int * int * string * Zone array)>()
        let slotZones = Array.zeroCreate<Zone array> slots.Count
        let slotMixed = Array.zeroCreate<bool> slots.Count
        let matchedExplicit = HashSet<struct(string * int)>()
        let routeStopField = field callSchema "route_stop_id"
        counts.["trip_call"] <-
            replace callPath (fun target ->
                use output = new ColumnWriter.Writer(target, callSchema, rowGroupRows, token, 64L * 1024L * 1024L)
                for columns in ColumnReader.groups callPath callSchema do
                    let trips = ColumnReader.text columns.[field callSchema "trip_id"]
                    let sequences = ColumnReader.int32 columns.[field callSchema "sequence"]
                    let places = ColumnReader.text columns.[field callSchema "location_id"]
                    let previous = ColumnReader.text columns.[routeStopField]
                    let updated = Array.zeroCreate<string> trips.Length
                    for row in 0 .. trips.Length - 1 do
                        let trip, sequence = trips.[row], sequences.[row]
                        let slot = slotOf trip sequence
                        updated.[row] <- slot |> Option.map (fun index -> slots.[index].id) |> Option.toObj
                        let old = previous.[row]
                        if not (isNull old) && wantedKeys.Contains(old) && slot.IsSome then
                            match slotsByKey.TryGetValue(old) with
                            | true, set -> set.Add(slot.Value) |> ignore
                            | _ -> slotsByKey.Add(old, SortedSet<int>([ slot.Value ]))
                            callByTripKey.TryAdd(struct(trip, old), struct(slot.Value, sequence)) |> ignore
                        let found =
                            match zones.calls.TryGetValue(struct(trip, sequence)) with
                            | true, values ->
                                matchedExplicit.Add(struct(trip, sequence)) |> ignore
                                values
                            | _ ->
                                match (if isNull old then (false, null) else zones.routeStops.TryGetValue(old)) with
                                | true, values -> values
                                | _ ->
                                    match zones.places.TryGetValue(places.[row]) with
                                    | true, values -> values
                                    | _ -> [||]
                        let slotIndex = slot |> Option.defaultValue -1
                        if slotIndex >= 0 then
                            match slotZones.[slotIndex] with
                            | null -> slotZones.[slotIndex] <- found
                            | existing when existing <> found -> slotMixed.[slotIndex] <- true
                            | _ -> ()
                        if found.Length > 0 then zonedCalls.Add(struct(trip, sequence, slotIndex, places.[row], found))
                    let copy = Array.copy columns
                    copy.[routeStopField] <- ColumnWriter.Text updated
                    // Re-batch: an input group may be larger than one bounded column batch.
                    for start in 0 .. rowGroupRows .. trips.Length - 1 do
                        output.Append(ColumnReader.slice start (min rowGroupRows (trips.Length - start)) copy)
                    progress "route-stops-rewrite-calls" output.RowCount
                int output.RowCount)

        // 8. Zones: route_stop_zone for unanimous slots, call_zone for the
        // other zoned calls, and location_zone as the distinct union.
        let unanimous index = index >= 0 && not slotMixed.[index] && not (isNull slotZones.[index]) && slotZones.[index].Length > 0
        let zoneSize (text: string) (zone: Zone) =
            96L + 2L * int64 (text.Length + zone.code.Length + (if isNull zone.system then 0 else zone.system.Length))
        let locationZones = ResizeArray<struct(string * Zone)>()
        let seenLocationZones = HashSet<struct(string * string * string)>()
        let addLocationZone location (zone: Zone) =
            if seenLocationZones.Add(struct(location, zone.code, zone.system)) then locationZones.Add(struct(location, zone))
        let routeStopZones = ResizeArray<struct(int * int * Zone)>()
        for index in 0 .. slots.Count - 1 do
            if unanimous index then
                slotZones.[index] |> Array.iteri (fun order zone ->
                    routeStopZones.Add(struct(index, order, zone))
                    addLocationZone slots.[index].location zone)
        let callZoneRows = ResizeArray<struct(string * int * int * Zone)>()
        for struct(trip, sequence, slot, location, found) in zonedCalls do
            if not (unanimous slot) then
                found |> Array.iteri (fun order zone ->
                    callZoneRows.Add(struct(trip, sequence, order, zone))
                    addLocationZone location zone)
        zonedCalls.Clear()
        counts.["route_stop_zone"] <-
            replace (Path.Combine(serving, "route_stop_zone.parquet")) (fun target ->
                RelationWriter.writeRows target (relation "route_stop_zone") (4L * 1024L * 1024L) rowGroupRows token progress
                    (fun (struct(slot: int, _, zone: Zone)) -> zoneSize slots.[slot].id zone)
                    (fun rows ->
                        let column f = Array.map f rows
                        [| ColumnWriter.Text(column (fun (struct(slot, _, _)) -> slots.[slot].route))
                           ColumnWriter.Text(column (fun (struct(slot, _, _)) -> slots.[slot].id))
                           ColumnWriter.Int32(column (fun (struct(_, order, _)) -> order))
                           ColumnWriter.Text(column (fun (struct(_, _, zone: Zone)) -> zone.code))
                           ColumnWriter.Text(column (fun (struct(_, _, zone: Zone)) -> zone.system)) |])
                    routeStopZones)
        counts.["call_zone"] <-
            replace (Path.Combine(serving, "call_zone.parquet")) (fun target ->
                RelationWriter.writeRows target (relation "call_zone") (4L * 1024L * 1024L) rowGroupRows token progress
                    (fun (struct(trip: string, _, _, zone: Zone)) -> zoneSize trip zone)
                    (fun rows ->
                        let column f = Array.map f rows
                        [| ColumnWriter.Text(column (fun (struct(trip, _, _, _)) -> trip))
                           ColumnWriter.Int32(column (fun (struct(_, sequence, _, _)) -> sequence))
                           ColumnWriter.Int32(column (fun (struct(_, _, order, _)) -> order))
                           ColumnWriter.Text(column (fun (struct(_, _, _, zone: Zone)) -> zone.code))
                           ColumnWriter.Text(column (fun (struct(_, _, _, zone: Zone)) -> zone.system)) |])
                    callZoneRows)
        counts.["location_zone"] <-
            replace (Path.Combine(serving, "location_zone.parquet")) (fun target ->
                RelationWriter.writeRows target (relation "location_zone") (4L * 1024L * 1024L) rowGroupRows token progress
                    (fun (struct(location: string, zone: Zone)) -> zoneSize location zone)
                    (fun rows ->
                        let column f = Array.map f rows
                        [| ColumnWriter.Text(column (fun (struct(location, _)) -> location))
                           ColumnWriter.Text(column (fun (struct(_, zone: Zone)) -> zone.code))
                           ColumnWriter.Text(column (fun (struct(_, zone: Zone)) -> if isNull zone.system then "" else zone.system)) |])
                    locationZones)

        // 9. Restrictions: a trip-scoped row points at its call; a
        // route-scoped row gets one row per slot its route stop reached
        // (directions split a JDF route stop into separate slots).
        let restrictionRows = ResizeArray<ColumnWriter.Column array>()
        let idField, keyField, tripField, sequenceField =
            field restrictionSchema "assignment_id", field restrictionSchema "route_stop_id",
            field restrictionSchema "trip_id", field restrictionSchema "call_sequence"
        let take (columns: ColumnWriter.Column array) row =
            columns |> Array.map (fun column ->
                match column with
                | ColumnWriter.Text values -> ColumnWriter.Text [| values.[row] |]
                | ColumnWriter.OptionalInt32 values -> ColumnWriter.OptionalInt32 [| values.[row] |]
                | ColumnWriter.Int32 values -> ColumnWriter.Int32 [| values.[row] |]
                | ColumnWriter.OptionalInt16 values -> ColumnWriter.OptionalInt16 [| values.[row] |]
                | other -> invalidOp $"Unexpected restriction column {other}")
        let set (columns: ColumnWriter.Column array) (id: string) (slot: string) (sequence: Nullable<int>) =
            columns.[idField] <- ColumnWriter.Text [| id |]
            columns.[keyField] <- ColumnWriter.Text [| slot |]
            columns.[sequenceField] <- ColumnWriter.OptionalInt32 [| sequence |]
            columns
        for columns in restrictionGroups do
            let ids = ColumnReader.text columns.[idField]
            let keys = ColumnReader.text columns.[keyField]
            let trips = ColumnReader.text columns.[tripField]
            let sequences = ColumnReader.optionalInt32 columns.[sequenceField]
            for row in 0 .. ids.Length - 1 do
                let key, trip = keys.[row], trips.[row]
                if not (isNull trip) then
                    let resolved =
                        if sequences.[row].HasValue then
                            slotOf trip sequences.[row].Value |> Option.map (fun slot -> struct(slot, sequences.[row].Value))
                        elif isNull key then None
                        else match callByTripKey.TryGetValue(struct(trip, key)) with | true, value -> Some value | _ -> None
                    match resolved with
                    | Some (struct(slot, sequence)) -> restrictionRows.Add(set (take columns row) ids.[row] slots.[slot].id (Nullable sequence))
                    | None -> restrictionRows.Add(set (take columns row) ids.[row] null sequences.[row])
                elif isNull key then restrictionRows.Add(take columns row)
                elif slotIds.Contains(key) then
                    // Already a slot (a base package row carried by an overlay).
                    restrictionRows.Add(take columns row)
                else
                    match slotsByKey.TryGetValue(key) with
                    | true, reached when reached.Count = 1 ->
                        restrictionRows.Add(set (take columns row) ids.[row] slots.[reached.Min].id (Nullable()))
                    | true, reached ->
                        for slot in reached do
                            let id = Identity.bindingId "restriction-slot" [ "assignment", ids.[row]; "route_stop", slots.[slot].id ]
                            restrictionRows.Add(set (take columns row) id slots.[slot].id (Nullable()))
                    | _ -> restrictionRows.Add(set (take columns row) ids.[row] null (Nullable()))
        counts.["travel_restriction_assignment"] <-
            replace restrictionPath (fun target ->
                use output = new ColumnWriter.Writer(target, restrictionSchema, rowGroupRows, token)
                let concat (batch: ColumnWriter.Column array array) index =
                    let pick f = batch |> Array.collect f
                    match batch.[0].[index] with
                    | ColumnWriter.Text _ -> ColumnWriter.Text (pick (fun row -> ColumnReader.text row.[index]))
                    | ColumnWriter.OptionalInt32 _ -> ColumnWriter.OptionalInt32 (pick (fun row -> ColumnReader.optionalInt32 row.[index]))
                    | other -> invalidOp $"Unexpected restriction column {other}"
                for batch in restrictionRows |> Seq.chunkBySize rowGroupRows do
                    output.Append(Array.init restrictionSchema.fields.Length (concat batch))
                int output.RowCount)
        {
            counts = counts
            summary = {
                routeDirections = groups.Count; patterns = patternCount; slots = slots.Count
                maximumPatterns = maximumPatterns; routeStopZones = routeStopZones.Count
                callZones = callZoneRows.Count; locationZones = locationZones.Count
                unmatchedCallZones = zones.calls.Count - matchedExplicit.Count }
        }
