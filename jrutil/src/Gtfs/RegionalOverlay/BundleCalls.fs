// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.BundleCalls

open System
open JrUtil.Hashing
open System.Collections.Generic
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
    let retainedCall ordinal copy (row: string array) (slice: TripSlice) =
        let outputRow = if copy then Array.copy row else row
        setCallField outputRow "trip_id" <| slice.trip.id
        match slice.selection with
        | Some selection when ordinal < selection.outputStopIds.Length ->
            if not (String.IsNullOrEmpty(selection.outputStopIds.[ordinal])) then
                setCallField outputRow "stop_id" <| selection.outputStopIds.[ordinal]
            selection.distances.[ordinal]
                |> Option.map (fun value -> value.ToString("G29", CultureInfo.InvariantCulture))
                |> Option.defaultValue ""
                |> setCallField outputRow "shape_dist_traveled"
            selection.arrivals.[ordinal] |> Option.iter (fun value -> setCallField outputRow "arrival_time" <| value)
            selection.departures.[ordinal] |> Option.iter (fun value -> setCallField outputRow "departure_time" <| value)
            match selection.sourceOrdinalByTarget.[ordinal] with
            | Some sourceOrdinal when projection.projectionsBySource.ContainsKey(selection.sourceTripId) ->
                let sourceCall = projection.projectionsBySource.[selection.sourceTripId].sourceCalls.[sourceOrdinal]
                sourceCall.stopHeadsign
                |> Option.iter (fun raw -> setCallField outputRow "stop_headsign" <| projection.fullHeadsign selection.sourceTripId sourceCall.sequence raw)
            | Some _ -> ()
            | None -> ()
        | _ -> ()
        outputRow
    let addedCall sourceTripId (sourceRow: string array) (addition: SourceTripAddition) =
        let outputRow = sourceRow
        setCallField outputRow "trip_id" <| addition.trip.id
        let rawHeadsign = getCallField sourceRow "stop_headsign"
        if not (String.IsNullOrWhiteSpace(rawHeadsign)) then
            setCallField outputRow "stop_headsign" <| projection.fullHeadsign sourceTripId (Int32.Parse(getCallField sourceRow "stop_sequence", CultureInfo.InvariantCulture)) rawHeadsign
        let sourceStopId = getCallField sourceRow "stop_id"
        setCallField outputRow "stop_id" <| projection.outputStopForSource.[sourceStopId]
        if addition.trip.shapeId.IsNone then setCallField outputRow "shape_dist_traveled" <| ""
        outputRow
    let mutable previousTrip = ""
    let mutable ordinal = 0
    let mutable outputBaseCallsRead = 0L
    let outputCalls = seq {
        for row in csvValues prepared.baseGtfs "stop_times.txt" stopTimeColumns do
            let baseTripId = getCallField row "trip_id"
            if baseTripId <> previousTrip then
                previousTrip <- baseTripId
                ordinal <- 0
            match projection.slicesByBaseTrip.TryGetValue(baseTripId) with
            | true, slices ->
                for slice in slices do
                    let outputRow = retainedCall ordinal (slices.Length > 1) row slice
                    usedStopIds.Add(getCallField outputRow "stop_id") |> ignore
                    writtenCallCount <- writtenCallCount + 1L
                    yield outputRow
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
    let compareCalls left right =
        let trip = StringComparer.Ordinal.Compare(getCallField left "trip_id", getCallField right "trip_id")
        if trip <> 0 then trip
        else compare (Int32.Parse(getCallField left "stop_sequence", CultureInfo.InvariantCulture))
                     (Int32.Parse(getCallField right "stop_sequence", CultureInfo.InvariantCulture))
    do
        use scratch = new Scratch.Storage(sibling)
        logProgress "sort-output-calls" 0L None
        let ordered = Scratch.sortRows scratch compareCalls Scratch.defaultBufferBytes outputCalls
        let mutable written = 0L
        for row in ordered do
            writeCsvRow stopTimeWriter row
            written <- written + 1L
            if written % 1000000L = 0L then logProgress "write-sorted-output-calls" written (Some writtenCallCount)
        logProgress "write-sorted-output-calls" written (Some writtenCallCount)
    stopTimeWriter.Flush()
    stopTimeWriter.Dispose()
    logProgress "write-output-calls" outputBaseCallsRead (Some outputBaseCallsRead)
