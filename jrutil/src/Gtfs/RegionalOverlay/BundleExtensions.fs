// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.BundleExtensions

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

/// Write the Czech extension tables (cz_routes, cz_trips, cz_stops, stop zones).
let write (context: Context) (zones: StopZones) =
    let prepared = context.prepared
    let projection = context.projection
    let source = context.source
    let extensionsOutput = context.extensionsOutput
    let usedStopIds = context.usedStopIds
    let usedRouteIds = context.usedRouteIds
    let regionalZonesByPlace = zones.regionalZonesByPlace
    let outputZoneIdentity = zones.outputZoneIdentity
    let isUsedPlace = zones.isUsedPlace
    // Each read opens and parses the base table, so read the columns once.
    let czRouteColumns = columnsOf prepared.baseExtensions "cz_routes.txt"
    let czTripColumns = columnsOf prepared.baseExtensions "cz_trips.txt"
    let czStopColumns = columnsOf prepared.baseExtensions "cz_stops.txt"
    let outputCzRouteRows =
        seq {
            yield! prepared.baseCzRouteRows |> Seq.filter (fun row -> usedRouteIds.Contains(rowValue row "route_id"))
            for KeyValue(outputRouteId, (sourceRoute, cisLineId)) in source.nativeRoutes do
                if usedRouteIds.Contains(outputRouteId) then
                    let row = CsvRow(StringComparer.Ordinal)
                    for column in czRouteColumns do row.[column] <- ""
                    row.["route_id"] <- outputRouteId
                    row.["cis_line_id"] <- cisLineId
                    row.["public_line_number"] <- rowValue sourceRoute "route_short_name"
                    row.["source_provenance"] <- sourceIdentity prepared.binding.sourceId sourceRoute
                    yield row
        }
        |> Seq.sortBy (fun row -> rowValue row "route_id")
        |> Seq.toArray
    writeRows (Path.Combine(extensionsOutput, "cz_routes.txt")) (czRouteColumns) outputCzRouteRows
    let mutable writtenCzTrips = 0L
    let outputCzTripRows = seq {
        for baseRow in csvRows prepared.baseExtensions "cz_trips.txt" do
            let baseTripId = rowValue baseRow "trip_id"
            match projection.slicesByBaseTrip.TryGetValue(baseTripId) with
            | true, slices ->
                for slice in slices do
                    writtenCzTrips <- writtenCzTrips + 1L
                    if writtenCzTrips % 100000L = 0L then
                        logProgress "write-cz-trips" writtenCzTrips None
                    yield cloneRow baseRow |> setRow <| "trip_id" <| slice.trip.id
            | _ -> ()
        for addition in projection.sourceTripAdditions |> Array.sortBy (fun value -> value.trip.id) do
            writtenCzTrips <- writtenCzTrips + 1L
            let row = CsvRow(StringComparer.Ordinal)
            for column in czTripColumns do row.[column] <- ""
            row.["trip_id"] <- addition.trip.id
            row.["cis_line_id"] <- if addition.projection.cisLineId.StartsWith("source:", StringComparison.Ordinal) then "" else addition.projection.cisLineId
            row.["source_trip_ids"] <- addition.projection.sourceTripReferences |> Array.map (fun (sourceId, tripId) -> sourceId + ":" + tripId) |> String.concat ";"
            row.["coverage_sources"] <- String.concat ";" addition.projection.sourceIds
            yield row
    }
    writeRows (Path.Combine(extensionsOutput, "cz_trips.txt")) (czTripColumns) outputCzTripRows
    logProgress "write-cz-trips" writtenCzTrips (Some writtenCzTrips)
    let outputCzStopRows = ResizeArray<CsvRow>()
    for baseRow in csvRows prepared.baseExtensions "cz_stops.txt" do
        if usedStopIds.Contains(rowValue baseRow "stop_id") then outputCzStopRows.Add(baseRow)
    for KeyValue(outputPlaceId, group) in source.nativeStopPlaces do
        if usedStopIds.Contains(outputPlaceId) then
            let row = CsvRow(StringComparer.Ordinal)
            for column in czStopColumns do row.[column] <- ""
            row.["stop_id"] <- outputPlaceId
            row.["stop_place_id"] <- outputPlaceId
            row.["asw_id"] <- group.members |> Array.map (fun memberRow -> rowValue memberRow prepared.policy.source.stopMatch.groupColumn) |> Array.distinct |> String.concat ";"
            row.["source_ids"] <- group.members |> Array.map (fun memberRow -> sourceIdentity prepared.binding.sourceId memberRow + ":" + originalIdentity "stop_id" memberRow) |> String.concat ";"
            outputCzStopRows.Add(row)
    for sourceStopId in projection.acceptedSourceStopIds |> Set.toArray |> Array.sort do
        match projection.outputStopForSource.TryGetValue(sourceStopId) with
        | true, outputStopId when usedStopIds.Contains(outputStopId) && outputStopId <> source.mappedPlaceBySourceStop.[sourceStopId] ->
            let row = CsvRow(StringComparer.Ordinal)
            for column in czStopColumns do row.[column] <- ""
            row.["stop_id"] <- outputStopId
            row.["stop_place_id"] <- source.mappedPlaceBySourceStop.[sourceStopId]
            row.["post_id"] <- optionText (rowValue projection.sourceStops.[sourceStopId] prepared.policy.source.stopMatch.postColumn) |> Option.defaultValue (originalIdentity "stop_id" projection.sourceStops.[sourceStopId])
            row.["asw_id"] <- rowValue projection.sourceStops.[sourceStopId] prepared.policy.source.stopMatch.groupColumn
            row.["source_ids"] <- sourceIdentity prepared.binding.sourceId projection.sourceStops.[sourceStopId] + ":" + originalIdentity "stop_id" projection.sourceStops.[sourceStopId]
            outputCzStopRows.Add(row)
        | _ -> ()
    outputCzStopRows
    |> Seq.distinctBy (fun row -> rowValue row "stop_id")
    |> Seq.sortBy (fun row -> rowValue row "stop_id")
    |> writeRows (Path.Combine(extensionsOutput, "cz_stops.txt")) (czStopColumns)

    let stopZonePath = Path.Combine(prepared.baseExtensions, "cz_stop_zones.txt")
    if File.Exists(stopZonePath) then
        let stopZoneColumns =
            seq {
                yield! columnsOf prepared.baseExtensions "cz_stop_zones.txt"
                yield! [ "stop_place_id"; "zone_id"; "zone_code"; "route_id"; "ids_system_id"; "source_provenance" ]
            }
            |> Seq.distinct
            |> Seq.toArray
        seq {
            yield!
                csvRows prepared.baseExtensions "cz_stop_zones.txt"
                |> Seq.filter (fun row -> usedStopIds.Contains(rowValue row "stop_place_id") && (String.IsNullOrWhiteSpace(rowValue row "route_id") || usedRouteIds.Contains(rowValue row "route_id")))
            for KeyValue(stopPlaceId, zones) in regionalZonesByPlace do
                if isUsedPlace stopPlaceId then
                    for code in zones do
                        let row = CsvRow(StringComparer.Ordinal)
                        for column in stopZoneColumns do row.[column] <- ""
                        row.["stop_place_id"] <- stopPlaceId
                        row.["zone_id"] <- outputZoneIdentity code
                        row.["zone_code"] <- code
                        row.["ids_system_id"] <- "ids-jmk"
                        row.["source_provenance"] <- "ids-jmk-gtfs"
                        yield row
        }
        |> Seq.distinctBy (fun row -> String.concat "\u001f" [ rowValue row "stop_place_id"; rowValue row "zone_id"; rowValue row "route_id"; rowValue row "ids_system_id" ])
        |> Seq.sortBy (fun row -> rowValue row "stop_place_id", rowValue row "zone_id", rowValue row "route_id")
        |> writeRows (Path.Combine(extensionsOutput, "cz_stop_zones.txt")) stopZoneColumns
    let tripStopZonePath = Path.Combine(prepared.baseExtensions, "cz_trip_stop_zones.txt")
    if File.Exists(tripStopZonePath) then
        seq {
            for row in csvRows prepared.baseExtensions "cz_trip_stop_zones.txt" do
                let baseTripId = rowValue row "trip_id"
                match projection.slicesByBaseTrip.TryGetValue(baseTripId) with
                | true, slices ->
                    for slice in slices do yield cloneRow row |> setRow <| "trip_id" <| slice.trip.id
                | _ -> ()
        }
        |> writeRows (Path.Combine(extensionsOutput, "cz_trip_stop_zones.txt")) (columnsOf prepared.baseExtensions "cz_trip_stop_zones.txt")
