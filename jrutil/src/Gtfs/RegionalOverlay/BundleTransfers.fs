// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.BundleTransfers

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

/// Project national and source transfers onto output trips.
let write (context: Context) =
    let prepared = context.prepared
    let projection = context.projection
    let source = context.source
    let matches = context.matches
    let gtfsOutput = context.gtfsOutput
    let usedStopIds = context.usedStopIds
    let bindingsBySourceTrip = matches.bindings |> Seq.groupBy (fun value -> value.sourceTripId) |> dict
    let slicesForBinding (matchBinding: MatchBinding) =
        match projection.slicesByBaseTrip.TryGetValue(matchBinding.targetTripId) with
        | true, slices ->
            let bindingSelection = projection.selectionByBinding.[projection.bindingKey matchBinding]
            slices
            |> Array.choose (fun slice ->
                match slice.selection with
                | Some selected when projection.selectionsCompatible selected bindingSelection ->
                    let dates = dateIntersection matchBinding.dates slice.dates
                    if anyDate dates then Some (slice, dates) else None
                | _ -> None)
        | _ -> [||]
    logProgress "write-transfers" 0L None
    let transferColumns = [|
        "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"
        "from_trip_id"; "to_trip_id"; "transfer_type"; "min_transfer_time"; "max_waiting_time"
    |]
    let transferRows = ResizeArray<CsvRow>()
    let transferProvenance = ResizeArray<string array>()
    for baseTransfer in csvRows prepared.baseGtfs "transfers.txt" do
        let fromTrip = optionText (rowValue baseTransfer "from_trip_id")
        let toTrip = optionText (rowValue baseTransfer "to_trip_id")
        let fromSlices =
            match fromTrip with
            | Some id when projection.slicesByBaseTrip.ContainsKey(id) -> projection.slicesByBaseTrip.[id] |> Array.map Some
            | Some _ -> [||]
            | None -> [| None |]
        let toSlices =
            match toTrip with
            | Some id when projection.slicesByBaseTrip.ContainsKey(id) -> projection.slicesByBaseTrip.[id] |> Array.map Some
            | Some _ -> [||]
            | None -> [| None |]
        for fromSlice in fromSlices do
            for toSlice in toSlices do
                let row = cloneRow baseTransfer
                fromSlice |> Option.iter (fun slice -> row.["from_trip_id"] <- slice.trip.id)
                toSlice |> Option.iter (fun slice -> row.["to_trip_id"] <- slice.trip.id)
                let stopsSurvive =
                    [ "from_stop_id"; "to_stop_id" ]
                    |> List.forall (fun column ->
                        match optionText (rowValue row column) with
                        | Some id -> usedStopIds.Contains(id)
                        | None -> true)
                if stopsSurvive then transferRows.Add(row)

    let transferOutputsForSourceTrip sourceTripId =
        seq {
            match bindingsBySourceTrip.TryGetValue(sourceTripId) with
            | true, sourceBindings ->
                for sourceBinding in sourceBindings do
                    if projection.bindingEligible "transfers" sourceBinding then
                        for slice, dates in slicesForBinding sourceBinding do
                            yield slice.trip.id, dates
            | _ -> ()
            match projection.sourceTripAdditionsBySourceId.TryGetValue(sourceTripId) with
            | true, addition -> yield addition.trip.id, addition.projection.dates
            | _ -> ()
        }
        |> Seq.toArray
    if enabled prepared.policy "transfers" then
        for sourceTransfer in csvRows prepared.binding.payloadPath "transfers.txt" do
            let fromSourceTrip = optionText (rowValue sourceTransfer "from_trip_id")
            let toSourceTrip = optionText (rowValue sourceTransfer "to_trip_id")
            match fromSourceTrip, toSourceTrip with
            | None, None ->
                transferProvenance.Add([|
                    rowValue sourceTransfer "from_stop_id"; rowValue sourceTransfer "to_stop_id"
                    "stop_only_not_projected"; "Standard GTFS cannot express date-bounded stop-only validity"
                |])
            | Some fromId, Some toId ->
                let fromOutputStop =
                    optionText (rowValue sourceTransfer "from_stop_id")
                    |> Option.bind (fun id -> match projection.outputStopForSource.TryGetValue(id) with | true, value -> Some value | _ -> None)
                    |> Option.filter usedStopIds.Contains
                let toOutputStop =
                    optionText (rowValue sourceTransfer "to_stop_id")
                    |> Option.bind (fun id -> match projection.outputStopForSource.TryGetValue(id) with | true, value -> Some value | _ -> None)
                    |> Option.filter usedStopIds.Contains
                let fromOutputs = transferOutputsForSourceTrip fromId
                let toOutputs = transferOutputsForSourceTrip toId
                if fromOutputs.Length = 0 || toOutputs.Length = 0 then
                    addDiagnostic prepared.diagnostics "transfer_reference_unresolved" (fromId + "->" + toId) "Both non-rail source trips and their output slices must resolve"
                else
                    for fromTripId, fromDates in fromOutputs do
                        for toTripId, toDates in toOutputs do
                            let applicableDates = dateIntersection fromDates toDates
                            if anyDate applicableDates && fromOutputStop.IsSome && toOutputStop.IsSome then
                                let row = CsvRow(StringComparer.Ordinal)
                                for column in transferColumns do row.[column] <- ""
                                row.["from_stop_id"] <- fromOutputStop |> Option.defaultValue ""
                                row.["to_stop_id"] <- toOutputStop |> Option.defaultValue ""
                                row.["from_trip_id"] <- fromTripId
                                row.["to_trip_id"] <- toTripId
                                row.["transfer_type"] <- rowValue sourceTransfer "transfer_type"
                                row.["min_transfer_time"] <- rowValue sourceTransfer "min_transfer_time"
                                row.["max_waiting_time"] <- rowValue sourceTransfer "max_waiting_time"
                                transferRows.Add(row)
                                transferProvenance.Add([|
                                    rowValue sourceTransfer "from_stop_id"; rowValue sourceTransfer "to_stop_id"
                                    "trip_specific_projected"; fromTripId + "->" + toTripId
                                |])
            | _ ->
                addDiagnostic prepared.diagnostics "transfer_reference_unresolved"
                    (rowValue sourceTransfer "from_trip_id" + "->" + rowValue sourceTransfer "to_trip_id")
                    "Both non-rail source trips and their output slices must resolve"
    if transferRows.Count > 0 then
        let reconciledTransfers, transferDiagnostics = reconcileTransferRows transferRows
        for code, objectId, message in transferDiagnostics do
            addDiagnostic prepared.diagnostics code objectId message
        reconciledTransfers
        |> Seq.sortBy (fun row -> rowValue row "from_trip_id", rowValue row "to_trip_id", rowValue row "from_stop_id", rowValue row "to_stop_id")
        |> writeRows (Path.Combine(gtfsOutput, "transfers.txt")) transferColumns
    { rows = transferRows; provenance = transferProvenance; slicesForBinding = slicesForBinding }
