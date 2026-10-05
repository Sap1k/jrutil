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
        unmatchedCallZones: int
    }

    type Result = { counts: IDictionary<string, int>; summary: Summary }

    [<Struct>]
    type private Call = { sequence: int; location: int; time: int; zones: int }

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
        // Zone sets are interned; index 0 is "no zones".
        let zoneSets = ResizeArray<Zone array>([ [||] ])
        let zoneSetIndex = Dictionary<string, int>(StringComparer.Ordinal)
        let internZones (values: Zone array) =
            if values.Length = 0 then 0 else
            let key = values |> Array.map (fun zone -> zone.code + "\u001f" + (if isNull zone.system then "" else zone.system)) |> String.concat "\u001e"
            match zoneSetIndex.TryGetValue(key) with
            | true, index -> index
            | _ ->
                zoneSetIndex.Add(key, zoneSets.Count)
                zoneSets.Add(values)
                zoneSets.Count - 1
        let matchedExplicit = HashSet<struct(string * int)>()
        let callsByTrip = Dictionary<string, ResizeArray<Call>>(StringComparer.Ordinal)
        let mutable read = 0L
        for columns in ColumnReader.groups callPath callSchema do
            let trips = ColumnReader.text columns.[field callSchema "trip_id"]
            let sequences = ColumnReader.int32 columns.[field callSchema "sequence"]
            let places = ColumnReader.text columns.[field callSchema "location_id"]
            let arrivals = ColumnReader.optionalInt32 columns.[field callSchema "scheduled_arrival"]
            let departures = ColumnReader.optionalInt32 columns.[field callSchema "scheduled_departure"]
            let previous = ColumnReader.text columns.[field callSchema "route_stop_id"]
            let passenger = match columns.[field callSchema "passenger_service"] with
                            | ColumnWriter.Boolean values -> values
                            | _ -> invalidOp "Expected trip_call.passenger_service"
            // Non-passenger calls (CZPTT railway points) have no route stop.
            for row in 0 .. trips.Length - 1 do
              if passenger.[row] then
                // An explicit call zone wins, then the call's source route
                // stop, then its stop place.
                let found =
                    match zones.calls.TryGetValue(struct(trips.[row], sequences.[row])) with
                    | true, values ->
                        matchedExplicit.Add(struct(trips.[row], sequences.[row])) |> ignore
                        values
                    | _ ->
                        let old = previous.[row]
                        match (if isNull old then (false, null) else zones.routeStops.TryGetValue(old)) with
                        | true, values -> values
                        | _ ->
                            match zones.places.TryGetValue(places.[row]) with
                            | true, values -> values
                            | _ -> [||]
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
                calls.Add({ sequence = sequences.[row]; location = intern places.[row]; time = time; zones = internZones found })
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
        // A slot keeps zones only when every call at it has the same
        // non-empty set; other zoned calls keep their own rows.
        let slotZones = Array.create slots.Count -1
        let slotMixed = Array.zeroCreate<bool> slots.Count
        for KeyValue(trip, calls) in callsByTrip do
            let struct(pattern, _) = tripPattern.[trip]
            let assigned = slotsByPattern.[pattern]
            for position in 0 .. calls.Count - 1 do
                let slot, set = assigned.[position], calls.[position].zones
                if slotZones.[slot] = -1 then slotZones.[slot] <- set
                elif slotZones.[slot] <> set then slotMixed.[slot] <- true
        let unanimous slot = not slotMixed.[slot] && slotZones.[slot] > 0
        let routeStopZones = ResizeArray<struct(int * int * Zone)>()
        for slot in 0 .. slots.Count - 1 do
            if unanimous slot then
                zoneSets.[slotZones.[slot]] |> Array.iteri (fun order zone ->
                    routeStopZones.Add(struct(slot, order, zone)))
        let callZoneRows = ResizeArray<struct(string * int * int * Zone)>()
        for KeyValue(trip, calls) in callsByTrip do
            let struct(pattern, _) = tripPattern.[trip]
            let assigned = slotsByPattern.[pattern]
            for position in 0 .. calls.Count - 1 do
                let call = calls.[position]
                if call.zones > 0 && not (unanimous assigned.[position]) then
                    zoneSets.[call.zones] |> Array.iteri (fun order zone ->
                        callZoneRows.Add(struct(trip, call.sequence, order, zone)))
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
                        [| ColumnWriter.Text(column _.id); ColumnWriter.Text(column _.route)
                           ColumnWriter.OptionalInt16(column _.direction); ColumnWriter.Int32(column _.sequence)
                           ColumnWriter.Text(column _.location) |])
                    slots)

        // 6. Restriction rows that name a compiler route-stop key.
        let restrictionPath = Path.Combine(serving, "travel_restriction.parquet")
        let restrictionSchema = relation "travel_restriction"
        let restrictionGroups = ColumnReader.groups restrictionPath restrictionSchema |> Seq.toArray
        let wantedKeys = HashSet<string>(StringComparer.Ordinal)
        for columns in restrictionGroups do
            for key in ColumnReader.text columns.[field restrictionSchema "route_stop_id"] do
                if not (isNull key) then wantedKeys.Add(key) |> ignore
        let slotsByKey = Dictionary<string, SortedSet<int>>(StringComparer.Ordinal)
        let callByTripKey = Dictionary<struct(string * string), struct(int * int)>()
        let slotIds = HashSet<string>(slots |> Seq.map _.id, StringComparer.Ordinal)

        // 7. Rewrite trip_call with slot ids.
        let routeStopField = field callSchema "route_stop_id"
        counts.["trip_call"] <-
            replace callPath (fun target ->
                use output = new ColumnWriter.Writer(target, callSchema, rowGroupRows, token, 64L * 1024L * 1024L)
                for columns in ColumnReader.groups callPath callSchema do
                    let trips = ColumnReader.text columns.[field callSchema "trip_id"]
                    let sequences = ColumnReader.int32 columns.[field callSchema "sequence"]
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
                    let copy = Array.copy columns
                    copy.[routeStopField] <- ColumnWriter.Text updated
                    // Re-batch: an input group may be larger than one bounded column batch.
                    for start in 0 .. rowGroupRows .. trips.Length - 1 do
                        output.Append(ColumnReader.slice start (min rowGroupRows (trips.Length - start)) copy)
                    progress "route-stops-rewrite-calls" output.RowCount
                int output.RowCount)

        // 8. Zones.
        let zoneSize (text: string) (zone: Zone) =
            96L + 2L * int64 (text.Length + zone.code.Length + (if isNull zone.system then 0 else zone.system.Length))
        counts.["route_stop_zone"] <-
            replace (Path.Combine(serving, "route_stop_zone.parquet")) (fun target ->
                RelationWriter.writeRows target (relation "route_stop_zone") (4L * 1024L * 1024L) rowGroupRows token progress
                    (fun (struct(slot: int, _, zone: Zone)) -> zoneSize slots.[slot].id zone)
                    (fun rows ->
                        let column f = Array.map f rows
                        [| ColumnWriter.Text(column (fun (struct(slot, _, _)) -> slots.[slot].id))
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

        // 9. Restrictions: a trip-scoped row points at its call; a
        // route-scoped row gets one row per slot its route stop reached
        // (directions split a JDF route stop into separate slots).
        let restrictionRows = ResizeArray<ColumnWriter.Column array>()
        let idField, keyField, tripField, sequenceField =
            field restrictionSchema "restriction_id", field restrictionSchema "route_stop_id",
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
                            let feed = ids.[row].Substring(0, max 0 (ids.[row].IndexOf(':')))
                            let id = Identity.feedId feed "restriction-slot" [ "assignment", ids.[row]; "route_stop", slots.[slot].id ]
                            restrictionRows.Add(set (take columns row) id slots.[slot].id (Nullable()))
                    | _ -> restrictionRows.Add(set (take columns row) ids.[row] null (Nullable()))
        counts.["travel_restriction"] <-
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
                callZones = callZoneRows.Count
                unmatchedCallZones = zones.calls.Count - matchedExplicit.Count }
        }
