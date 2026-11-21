// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.BundleCalls

open System
open System.Globalization
open System.IO
open System.Text

open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Runtime
open JrUtil.RegionalOverlay.Values
open JrUtil.RegionalOverlay.GtfsFiles
open JrUtil.RegionalOverlay.GtfsRows
open JrUtil.RegionalOverlay.StopGroups
open JrUtil.RegionalOverlay.OutputIds
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay
open JrUtil.RegionalOverlay.BundleOutput

/// Stream national and added calls, sorted by trip and stop sequence, into stop_times.txt.
let write (context: Context) sibling =
    let prepared = context.prepared
    let projection = context.projection
    let gtfsOutput = context.gtfsOutput
    let usedStopIds = context.usedStopIds
    let mutable writtenCallCount = 0L
    logProgress "write-output-calls" 0L None
    let stopTimeColumns = columnsOf prepared.baseGtfs "stop_times.txt"
    let stopTimeIndexes = stopTimeColumns |> Array.mapi (fun index column -> column, index) |> dict
    let getCallField (row: string array) column =
        match stopTimeIndexes.TryGetValue(column) with | true, index -> row.[index] | _ -> ""
    let setCallField (row: string array) column value =
        match stopTimeIndexes.TryGetValue(column) with | true, index -> row.[index] <- value | _ -> ()
    let stopTimeOutput = Path.Combine(gtfsOutput, "stop_times.txt")
    use stopTimeWriter = new StreamWriter(stopTimeOutput, false, new UTF8Encoding(false))
    stopTimeWriter.NewLine <- "\n"
    writeCsvRow stopTimeWriter stopTimeColumns
    let sourceHeadsign (selection: OverlaySelection) sourceOrdinal (outputRow: string array) =
        if projection.projectionsBySource.ContainsKey(selection.sourceTripId) then
            let sourceCall = projection.projectionsBySource.[selection.sourceTripId].sourceCalls.[sourceOrdinal]
            sourceCall.stopHeadsign
            |> Option.iter (fun raw -> setCallField outputRow "stop_headsign" <| projection.fullHeadsign selection.sourceTripId sourceCall.sequence raw)
    /// The calls a source repeats at the stop place of this base call, as rows
    /// cloned from the base row.
    let repeatedCalls ordinal (row: string array) (slice: TripSlice) =
        match slice.selection with
        | Some selection when selection.repeats.Length > 0 ->
            selection.repeats
            |> Array.filter (fun call -> call.targetOrdinal = ordinal)
            |> Array.map (fun call ->
                let outputRow = Array.copy row
                setCallField outputRow "trip_id" <| slice.trip.id
                if not (String.IsNullOrEmpty(call.outputStopId)) then setCallField outputRow "stop_id" <| call.outputStopId
                call.distance
                    |> Option.iter (fun value -> setCallField outputRow "shape_dist_traveled" <| value.ToString("G29", CultureInfo.InvariantCulture))
                setCallField outputRow "arrival_time" <| call.arrival
                setCallField outputRow "departure_time" <| call.departure
                sourceHeadsign selection call.sourceOrdinal outputRow
                outputRow)
        | _ -> [||]
    let retainedCall ordinal copy (row: string array) (slice: TripSlice) =
        let outputRow = if copy then Array.copy row else row
        setCallField outputRow "trip_id" <| slice.trip.id
        match slice.selection with
        | Some selection when ordinal < selection.outputStopIds.Length ->
            if not (String.IsNullOrEmpty(selection.outputStopIds.[ordinal])) then
                setCallField outputRow "stop_id" <| selection.outputStopIds.[ordinal]
            // A base call keeps its JDF kilometre value unless the source supplies a distance.
            selection.distances.[ordinal]
                |> Option.iter (fun value -> setCallField outputRow "shape_dist_traveled" <| value.ToString("G29", CultureInfo.InvariantCulture))
            selection.arrivals.[ordinal] |> Option.iter (fun value -> setCallField outputRow "arrival_time" <| value)
            selection.departures.[ordinal] |> Option.iter (fun value -> setCallField outputRow "departure_time" <| value)
            selection.sourceOrdinalByTarget.[ordinal] |> Option.iter (fun sourceOrdinal -> sourceHeadsign selection sourceOrdinal outputRow)
        | _ -> ()
        outputRow
    let addedCall sourceTripId (sourceRow: string array) (addition: SourceTripAddition) =
        let outputRow = sourceRow
        setCallField outputRow "trip_id" <| addition.trip.id
        let sequence = Int32.Parse(getCallField sourceRow "stop_sequence", CultureInfo.InvariantCulture)
        // Times come from the projected calls, not the raw row: a trip folded onto the next
        // day (PID 24:30 → 00:30, foldAfterMidnightTrips) carries its next-day service.
        addition.projection.sourceCalls
        |> Array.tryFind (fun call -> call.sequence = sequence)
        |> Option.iter (fun call ->
            setCallField outputRow "arrival_time" call.arrival
            setCallField outputRow "departure_time" call.departure)
        let rawHeadsign = getCallField sourceRow "stop_headsign"
        if not (String.IsNullOrWhiteSpace(rawHeadsign)) then
            setCallField outputRow "stop_headsign" <| projection.fullHeadsign sourceTripId sequence rawHeadsign
        let sourceStopId = getCallField sourceRow "stop_id"
        setCallField outputRow "stop_id" <| projection.outputStopForSource.[sourceStopId]
        if addition.trip.shapeId.IsNone then setCallField outputRow "shape_dist_traveled" <| ""
        outputRow
    let mutable previousTrip = ""
    let mutable ordinal = 0
    // Repeated calls already written for each slice of the current base trip.
    let mutable repeatsWritten: int array = [||]
    let mutable outputBaseCallsRead = 0L
    let outputCalls = seq {
        for row in csvValues prepared.baseGtfs "stop_times.txt" stopTimeColumns do
            let baseTripId = getCallField row "trip_id"
            if baseTripId <> previousTrip then
                previousTrip <- baseTripId
                ordinal <- 0
            match projection.slicesByBaseTrip.TryGetValue(baseTripId) with
            | true, slices ->
                if ordinal = 0 then repeatsWritten <- Array.zeroCreate slices.Length
                for sliceIndex in 0 .. slices.Length - 1 do
                    let slice = slices.[sliceIndex]
                    let keepsRepeats = match slice.selection with | Some selection -> selection.repeats.Length > 0 | None -> false
                    if not keepsRepeats then
                        let outputRow = retainedCall ordinal (slices.Length > 1) row slice
                        usedStopIds.Add(getCallField outputRow "stop_id") |> ignore
                        writtenCallCount <- writtenCallCount + 1L
                        yield outputRow
                    else
                        // Later calls move back by the calls added before them.
                        let baseSequence = Int32.Parse(getCallField row "stop_sequence", CultureInfo.InvariantCulture)
                        let repeated = repeatedCalls ordinal row slice
                        let outputRow = retainedCall ordinal true row slice
                        let outputRows = Array.append [| outputRow |] repeated
                        let sequences = outputRows |> Array.mapi (fun index _ -> baseSequence + repeatsWritten.[sliceIndex] + index)
                        outputRows |> Array.iteri (fun index value -> setCallField value "stop_sequence" <| sequences.[index].ToString(CultureInfo.InvariantCulture))
                        repeatsWritten.[sliceIndex] <- repeatsWritten.[sliceIndex] + repeated.Length
                        let resequenced =
                            match context.resequencedCalls.TryGetValue(slice.trip.id) with
                            | true, values -> values
                            | _ ->
                                let values = System.Collections.Generic.Dictionary<int, int array>()
                                context.resequencedCalls.[slice.trip.id] <- values
                                values
                        resequenced.[baseSequence] <- sequences
                        for value in outputRows do
                            usedStopIds.Add(getCallField value "stop_id") |> ignore
                            writtenCallCount <- writtenCallCount + 1L
                            yield value
            | _ -> ()
            ordinal <- ordinal + 1
            outputBaseCallsRead <- outputBaseCallsRead + 1L
            if outputBaseCallsRead % 1000000L = 0L then logProgress "write-output-calls" outputBaseCallsRead None
        for sourceRow in csvValues prepared.binding.payloadPath "stop_times.txt" stopTimeColumns do
            let sourceTripId = getCallField sourceRow "trip_id"
            match projection.sourceTripAdditionsBySourceId.TryGetValue(sourceTripId) with
            | true, addition when sourceTripId = addition.projection.sourceTripId ->
                let outputRow = addedCall sourceTripId sourceRow addition
                usedStopIds.Add(getCallField outputRow "stop_id") |> ignore
                writtenCallCount <- writtenCallCount + 1L
                yield outputRow
            | _ -> ()
    }
    // Validity slices interleave output trip IDs. Normalize native output
    // once, before serialization, so every downstream call reader can stream.
    // Sequences are parsed once per row; an unparsable one keeps its text and
    // still fails, as Int32.Parse, when a comparison reaches it.
    let callKey row =
        let sequence = getCallField row "stop_sequence"
        match Int32.TryParse(sequence, NumberStyles.Integer, CultureInfo.InvariantCulture) with
        | true, value -> struct(getCallField row "trip_id", value, null)
        | _ -> struct(getCallField row "trip_id", 0, sequence)
    let compareCalls (struct(leftTrip, leftSequence, leftInvalid: string)) (struct(rightTrip, rightSequence, rightInvalid: string)) =
        let trip = StringComparer.Ordinal.Compare(leftTrip, rightTrip)
        if trip <> 0 then trip
        else
            if not (isNull leftInvalid) then Int32.Parse(leftInvalid, CultureInfo.InvariantCulture) |> ignore
            if not (isNull rightInvalid) then Int32.Parse(rightInvalid, CultureInfo.InvariantCulture) |> ignore
            compare leftSequence rightSequence
    do
        use scratch = new Scratch.Storage(sibling)
        logProgress "sort-output-calls" 0L None
        let ordered = Scratch.sortRowsBy scratch callKey compareCalls Scratch.defaultBufferBytes outputCalls
        let mutable written = 0L
        for row in ordered do
            writeCsvRow stopTimeWriter row
            written <- written + 1L
            if written % 1000000L = 0L then logProgress "write-sorted-output-calls" written (Some writtenCallCount)
        logProgress "write-sorted-output-calls" written (Some writtenCallCount)
    stopTimeWriter.Flush()
    stopTimeWriter.Dispose()
    logProgress "write-output-calls" outputBaseCallsRead (Some outputBaseCallsRead)
