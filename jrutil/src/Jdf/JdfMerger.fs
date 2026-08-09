// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.JdfMerger

open System
open System.Collections.Generic
open System.IO
open System.Text
open NodaTime
open Serilog
open NetTopologySuite.Geometries

open JrUtil.JdfModel
open JrUtil.JdfStopReconciliation
open JrUtil.JdfParser
open JrUtil.JdfSerializer
open JrUtil.Utils
open JrUtil.GeoData.Common

type private SpoolChunk = {
    path: string
    offset: int64
    length: int64
}

type private SpoolRoute = {
    storedDistinction: int
    chunks: ResizeArray<SpoolChunk>
}

type private SerializedBuffer = {
    stream: MemoryStream
    chunks: ((string * int) * int64 * int64) array
}

type private TripStopSpool(path: string) =
    let directory = Path.GetDirectoryName(path)
    do if not (String.IsNullOrEmpty(directory)) then Directory.CreateDirectory(directory) |> ignore

    let stream =
        new FileStream(
            path,
            FileMode.Create,
            FileAccess.ReadWrite,
            FileShare.Read,
            1024 * 1024,
            FileOptions.SequentialScan)
    let writer = new StreamWriter(stream, jdfEncoding, 1024 * 1024, true)
    let writeRecord = getJdfRecordWriter<TripStop>
    let routes = Dictionary<string * int, SpoolRoute>()
    let segmentPaths = HashSet<string>(StringComparer.OrdinalIgnoreCase)
    let mutable currentKey: (string * int) option = None
    let mutable currentOffset = 0L
    let mutable finalized = false
    let mutable peakLength = 0L

    let storedLength () =
        stream.Length
        + (segmentPaths
           |> Seq.sumBy (fun segmentPath ->
               if File.Exists(segmentPath) then FileInfo(segmentPath).Length else 0L))

    let finishChunk () =
        match currentKey with
        | Some key ->
            writer.Flush()
            let length = stream.Position - currentOffset
            if length > 0L then
                routes.[key].chunks.Add({ path = path; offset = currentOffset; length = length })
            currentKey <- None
        | None -> ()

    let ensureRoute key distinction =
        match routes.TryGetValue(key) with
        | true, route -> route
        | false, _ ->
            let route = { storedDistinction = distinction; chunks = ResizeArray() }
            routes.Add(key, route)
            route

    let finalize () =
        if not finalized then
            finishChunk ()
            writer.Flush()
            finalized <- true

    let copyChunk (input: Stream) (output: Stream) (chunk: SpoolChunk) =
        input.Position <- chunk.offset
        let buffer = Array.zeroCreate<byte> (1024 * 1024)
        let mutable remaining = chunk.length
        while remaining > 0L do
            let requested = int (min remaining (int64 buffer.Length))
            let read = input.Read(buffer, 0, requested)
            if read = 0 then raise (EndOfStreamException("Unexpected end of trip-stop spool"))
            output.Write(buffer, 0, read)
            remaining <- remaining - int64 read

    let rewriteChunk
        (input: Stream) (output: Stream) storedDistinction outputDistinction (chunk: SpoolChunk) =
        input.Position <- chunk.offset
        let oldSuffix = $"\",\"{storedDistinction}\";\r\n"
        let newSuffix = $"\",\"{outputDistinction}\";\r\n"
        let buffer = Array.zeroCreate<byte> (1024 * 1024)
        let mutable remaining = chunk.length
        let mutable carry = ""
        let mutable replaced = false
        let writeText (text: string) =
            let bytes = jdfEncoding.GetBytes(text)
            output.Write(bytes, 0, bytes.Length)
        while remaining > 0L do
            let requested = int (min remaining (int64 buffer.Length))
            let read = input.Read(buffer, 0, requested)
            if read = 0 then raise (EndOfStreamException("Unexpected end of trip-stop spool"))
            remaining <- remaining - int64 read
            let text = carry + jdfEncoding.GetString(buffer, 0, read)
            let safeLimit =
                if remaining = 0L then text.Length
                else max 0 (text.Length - oldSuffix.Length + 1)
            let mutable position = 0
            let mutable searching = true
            while searching do
                let found = text.IndexOf(oldSuffix, position, StringComparison.Ordinal)
                if found >= 0 && (remaining = 0L || found < safeLimit) then
                    writeText (text.Substring(position, found - position))
                    writeText newSuffix
                    replaced <- true
                    position <- found + oldSuffix.Length
                else
                    searching <- false
            let writeUntil = max position safeLimit
            writeText (text.Substring(position, writeUntil - position))
            carry <- text.Substring(writeUntil)
        if not replaced then
            failwithf "Trip-stop spool did not contain distinction %d" storedDistinction

    member _.Add(key: string * int, row: TripStop) =
        if finalized then invalidOp "Cannot append to a finalized trip-stop spool"
        ensureRoute key (snd key) |> ignore
        if currentKey <> Some key then
            finishChunk ()
            currentKey <- Some key
            currentOffset <- stream.Position
        writeRecord writer row

    member _.BeginMappedBatch(
        rows: TripStop array,
        workers: int,
        mapRow: TripStop -> TripStop) =
        if finalized then invalidOp "Cannot append to a finalized trip-stop spool"
        Threading.Tasks.Task.Run<unit -> unit>(Func<unit -> unit>(fun () ->
            let usefulPartitions = max 1 ((rows.Length + 2047) / 2048)
            let partitionCount =
                min workers (min Environment.ProcessorCount usefulPartitions)
            let partitionSize = (rows.Length + partitionCount - 1) / partitionCount
            let partitions =
                if rows.Length = 0 then [||]
                else [| for start in 0 .. partitionSize .. rows.Length - 1 ->
                          start, min rows.Length (start + partitionSize) |]
            let serializeBuffer (startIndex, endIndex) =
                let bufferStream = new MemoryStream()
                use partWriter = new StreamWriter(bufferStream, jdfEncoding, 64 * 1024, true)
                let chunks = ResizeArray<_>()
                let mutable chunkKey: (string * int) option = None
                let mutable chunkOffset = 0L
                let finishBufferChunk () =
                    match chunkKey with
                    | Some key ->
                        partWriter.Flush()
                        let length = bufferStream.Position - chunkOffset
                        if length > 0L then chunks.Add(key, chunkOffset, length)
                        chunkKey <- None
                    | None -> ()
                for index in startIndex .. endIndex - 1 do
                    let mapped = mapRow rows.[index]
                    let key = mapped.routeId, mapped.routeDistinction
                    if chunkKey <> Some key then
                        finishBufferChunk ()
                        chunkKey <- Some key
                        chunkOffset <- bufferStream.Position
                    writeRecord partWriter mapped
                finishBufferChunk ()
                partWriter.Flush()
                {
                    stream = bufferStream
                    chunks = chunks.ToArray()
                }
            let buffers =
                partitions
                |> mapParallelOrderedBatches partitionCount serializeBuffer
                |> Seq.toArray
            fun () ->
                if finalized then invalidOp "Cannot register into a finalized trip-stop spool"
                finishChunk ()
                writer.Flush()
                try
                    for buffer in buffers do
                        let baseOffset = stream.Position
                        buffer.stream.Position <- 0L
                        buffer.stream.CopyTo(stream)
                        for key, offset, length in buffer.chunks do
                            let route = ensureRoute key (snd key)
                            route.chunks.Add({
                                path = path
                                offset = baseOffset + offset
                                length = length
                            })
                    peakLength <- max peakLength (storedLength ())
                finally
                    for buffer in buffers do buffer.stream.Dispose()))

    member this.AddMappedBatch(
        rows: TripStop array,
        workers: int,
        mapRow: TripStop -> TripStop) =
        let register =
            this.BeginMappedBatch(rows, workers, mapRow).GetAwaiter().GetResult()
        register ()

    member _.Delete(key) =
        finishChunk ()
        routes.Remove(key) |> ignore

    member _.Copy(sourceKey, destinationKey) =
        finishChunk ()
        let source = ensureRoute sourceKey (snd sourceKey)
        routes.[destinationKey] <- source

    member _.WriteTo(output: Stream) =
        finalize ()
        let mutable segmentInput: FileStream = null
        let mutable segmentInputPath = ""
        let inputFor chunkPath =
            if String.Equals(chunkPath, path, StringComparison.OrdinalIgnoreCase) then
                stream :> Stream
            else
                if segmentInputPath <> chunkPath then
                    if not (isNull segmentInput) then segmentInput.Dispose()
                    segmentInput <- File.OpenRead(chunkPath)
                    segmentInputPath <- chunkPath
                segmentInput :> Stream
        try
            for KeyValue(key, route) in routes do
                for chunk in route.chunks do
                    let input = inputFor chunk.path
                    if route.storedDistinction = snd key then copyChunk input output chunk
                    else rewriteChunk input output route.storedDistinction (snd key) chunk
        finally
            if not (isNull segmentInput) then segmentInput.Dispose()

    member this.ToArray() =
        use memory = new MemoryStream()
        this.WriteTo(memory)
        memory.Position <- 0L
        let parser: Stream -> TripStop seq = getJdfParser
        parser memory |> Seq.toArray

    member _.Length =
        writer.Flush()
        storedLength ()

    member _.PeakLength =
        writer.Flush()
        max peakLength (storedLength ())

    interface IDisposable with
        member _.Dispose() =
            try
                try writer.Dispose()
                finally stream.Dispose()
            finally
                if File.Exists(path) then File.Delete(path)
                for segmentPath in segmentPaths do
                    if File.Exists(segmentPath) then File.Delete(segmentPath)

type StopMergeStrategy =
    // The default, should work on any valid JDF
    | MergeStopsByName
    // Can be convenient if you know all batches have consistent IDs
    | MergeStopsById

type JdfMerger(
    stopMergeStrategy: StopMergeStrategy,
    ?tripStopSpoolPath: string,
    ?tripStopTransformWorkers: int) =
    let stops = ResizeArray()
    let stopPosts = ResizeArray()
    let agenciesByIco = MultiDict()
    let routesByLicNum = MultiDict()
    let routeIntegrationsByRoute = MultiDict()
    let routeStopsByRoute = MultiDict()
    let tripsByRoute = MultiDict()
    let tripGroups = ResizeArray()
    let tripStopsByRoute = MultiDict()
    let tripStopSpool = tripStopSpoolPath |> Option.map (fun path -> new TripStopSpool(path))
    let tripStopTransformWorkers = defaultArg tripStopTransformWorkers 1
    do
        if tripStopTransformWorkers <= 0 then
            invalidArg "tripStopTransformWorkers" "Trip-stop transform workers must be positive"
    let routeInfoByRoute = MultiDict()
    let attributeRefs = ResizeArray()
    let serviceNotesByRoute = MultiDict()
    let transfersByRoute = MultiDict()
    let agencyAlternationsByRoute = MultiDict()
    let alternateRouteNamesByRoute = MultiDict()
    let reservationOptionsByRoute = MultiDict()
    let stopLocationsByStop = Dictionary()
    let stopLocationSourcesByStop = Dictionary<int64, string>()
    let stopIndexesById = Dictionary<int64, int>()
    let stopReconciler = StopReconciler()

    let mutable lastStopId = 0L
    let mutable lastAttributeRefId = 0
    let mutable lastTripGroupId = 0

    let attributeRefsByValue = Dictionary()
    let attributeArrays =
        Dictionary<int option array, int option array>(HashIdentity.Structural)
    let stopsByIds = Dictionary()
    let stopPostsSet = HashSet()
    let batchDateByRoute = Dictionary()

    let locationDistance loc1 loc2 =
        let locToPt loc =
            wgs84Factory.CreatePoint(
                Coordinate(float loc.lon, float loc.lat))
            |> pointWgs84ToEtrs89Ex
        (locToPt loc1).Distance(locToPt loc2)
    let locationDistanceThresh = 1000

    let mergeAttributes stopId (left: int option array) (right: int option array) =
        let merged =
            Seq.append (left |> Array.choose id) (right |> Array.choose id)
            |> Seq.distinct
            |> Seq.sort
            |> Seq.toArray
        if merged.Length > 6 then
            Log.Warning(
                "Merged stop {StopId} has {AttributeCount} attributes; JDF can retain only six",
                stopId,
                merged.Length)
        Array.init 6 (fun index ->
            if index < min 6 merged.Length then Some merged.[index] else None)

    member private this.batchWithTripStops tripStops = {
        version = {
            version = "1.11"
            duNum = None
            region = None
            batchId = None
            creationDate = Some <| dateToday ()
            generator = Some "JrUtil JdfMerger"
        }
        stops = stops |> Seq.toArray
        stopPosts = stopPosts |> Seq.toArray
        agencies = agenciesByIco.Values |> Seq.collect id |> Seq.toArray
        routes = routesByLicNum.Values |> Seq.collect id |> Seq.toArray
        routeIntegrations =
            routeIntegrationsByRoute.Values |> Seq.collect id |> Seq.toArray
        routeStops =
            routeStopsByRoute.Values |> Seq.collect id |> Seq.toArray
        trips = tripsByRoute.Values |> Seq.collect id |> Seq.toArray
        tripGroups = tripGroups |> Seq.toArray
        tripStops = tripStops
        routeInfo = routeInfoByRoute.Values |> Seq.collect id |> Seq.toArray
        attributeRefs = attributeRefs |> Seq.toArray
        serviceNotes =
            serviceNotesByRoute.Values |> Seq.collect id |> Seq.toArray
        transfers = transfersByRoute.Values |> Seq.collect id |> Seq.toArray
        agencyAlternations =
            agencyAlternationsByRoute.Values |> Seq.collect id |> Seq.toArray
        alternateRouteNames =
            alternateRouteNamesByRoute.Values |> Seq.collect id |> Seq.toArray
        reservationOptions =
            reservationOptionsByRoute.Values |> Seq.collect id |> Seq.toArray
        stopLocations = stopLocationsByStop.Values |> Seq.toArray
        stopLocationSources =
            stopLocationSourcesByStop
            |> Seq.map (fun pair -> { stopId = pair.Key; source = pair.Value })
            |> Seq.toArray
    }

    member this.batch =
        let tripStops =
            match tripStopSpool with
            | Some spool -> spool.ToArray()
            | None -> tripStopsByRoute.Values |> Seq.collect id |> Seq.toArray
        this.batchWithTripStops tripStops

    member this.write(path: string) =
        match tripStopSpool with
        | None -> Jdf.jdfBatchDirWriter () (Jdf.FsPath path) this.batch
        | Some spool ->
            let withoutTripStops = this.batchWithTripStops [||]
            Jdf.jdfBatchDirWriter () (Jdf.FsPath path) withoutTripStops
            use output = File.Open(Path.Combine(path, "Zasspoje.txt"), FileMode.Create)
            spool.WriteTo(output)

    member _.tripStopSpillBytes =
        tripStopSpool |> Option.map (fun spool -> spool.PeakLength) |> Option.defaultValue 0L

    member _.stopMergeStatistics = stopReconciler.Statistics

    member _.logStopMergeSummary() =
        let statistics = stopReconciler.Statistics
        Log.Information(
            "Stop reconciliation completed: {ExactCount} exact, {SuffixCount} suffix, "
            + "{FuzzyCount} fuzzy, {AmbiguousCount} ambiguous, "
            + "{CandidateComparisonCount} candidate comparisons "
            + "({FuzzyComparisonCount} fuzzy)",
            statistics.exact,
            statistics.suffix,
            statistics.fuzzy,
            statistics.ambiguous,
            statistics.candidateComparisons,
            statistics.fuzzyComparisons)

    member private this.deleteRoute(r: Route) =
        let routeId = r.id
        let routeDistinction = r.idDistinction
        routesByLicNum.[routeId].RemoveAll(fun r ->
            r.idDistinction = routeDistinction) |> ignore
        routeIntegrationsByRoute.Remove((routeId, routeDistinction)) |> ignore
        routeStopsByRoute.Remove((routeId, routeDistinction)) |> ignore
        tripsByRoute.Remove((routeId, routeDistinction)) |> ignore
        match tripStopSpool with
        | Some spool -> spool.Delete((routeId, routeDistinction))
        | None -> tripStopsByRoute.Remove((routeId, routeDistinction)) |> ignore
        routeInfoByRoute.Remove((routeId, routeDistinction)) |> ignore
        serviceNotesByRoute.Remove((routeId, routeDistinction)) |> ignore
        transfersByRoute.Remove((routeId, routeDistinction)) |> ignore
        agencyAlternationsByRoute.Remove((routeId, routeDistinction)) |> ignore
        alternateRouteNamesByRoute.Remove((routeId, routeDistinction)) |> ignore
        reservationOptionsByRoute.Remove((routeId, routeDistinction)) |> ignore
        batchDateByRoute.Remove((routeId, routeDistinction)) |> ignore

    member private this.copyRoute(copy: Route) =
        let oldDist = copy.idDistinction
        let newDist =
            (routesByLicNum.[copy.id]
             |> Seq.map (fun r -> r.idDistinction)
             |> Seq.max) + 1
        routesByLicNum.[copy.id].Add(
            {copy with idDistinction = newDist })
        routeIntegrationsByRoute.[(copy.id, newDist)] <-
            routeIntegrationsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        routeStopsByRoute.[(copy.id, newDist)] <-
            routeStopsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        tripsByRoute.[(copy.id, newDist)] <-
            tripsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        match tripStopSpool with
        | Some spool -> spool.Copy((copy.id, oldDist), (copy.id, newDist))
        | None ->
            tripStopsByRoute.[(copy.id, newDist)] <-
                tripStopsByRoute.[(copy.id, oldDist)]
                |> Seq.map (fun x -> {
                    x with
                        routeId = copy.id
                        routeDistinction = newDist
                })
        routeInfoByRoute.[(copy.id, newDist)] <-
            routeInfoByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        serviceNotesByRoute.[(copy.id, newDist)] <-
            serviceNotesByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        transfersByRoute.[(copy.id, newDist)] <-
            transfersByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        agencyAlternationsByRoute.[(copy.id, newDist)] <-
            agencyAlternationsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        alternateRouteNamesByRoute.[(copy.id, newDist)] <-
            alternateRouteNamesByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        reservationOptionsByRoute.[(copy.id, newDist)] <-
            reservationOptionsByRoute.[(copy.id, oldDist)]
            |> Seq.map (fun x -> {
                x with
                    routeId = copy.id
                    routeDistinction = newDist
            })
        batchDateByRoute.[(copy.id, newDist)] <-
            batchDateByRoute.[(copy.id, oldDist)]

    member private this.resolveOneRouteOverlap(r1, r2) =
        assert (r1.idDistinction <> r2.idDistinction)
        let r1Date = batchDateByRoute.[(r1.id, r1.idDistinction)]
        let r2Date = batchDateByRoute.[(r2.id, r2.idDistinction)]
        let r2Priority =
            (r2.detour && (not r1.detour || r1Date < r2Date))
            || (not r1.detour && r1Date < r2Date)
        let r1Priority = r1.detour && (not r2.detour || r2Date < r1Date)

        // Validity ranges don't overlap, keep both
        if r1.timetableValidTo < r2.timetableValidFrom
           || r1.timetableValidFrom > r2.timetableValidTo then ()
        // Ranges overlap exactly, keep only priority route
        else if r1.timetableValidFrom = r2.timetableValidFrom
             && r1.timetableValidTo = r2.timetableValidTo then
            let toDelete = if r2Priority then r1 else r2
            Log.Debug(
                "Route {LicNum} variants overlap exactly, removing {Dist}, \
                 keeping {Dist2}",
                r1.id, toDelete.idDistinction,
                (if r2Priority then r2 else r1).idDistinction)
            this.deleteRoute(toDelete)
        // Ranges start at the same day, cut one
        else if r1.timetableValidFrom = r2.timetableValidFrom
             && r1.timetableValidTo < r2.timetableValidTo then
            if r2Priority then
                Log.Debug("Route {LicNum} variants overlap with same start, \
                           removing {Dist}",
                          r1.id, r1.idDistinction)
                this.deleteRoute(r1)
            else
                Log.Debug("Route {LicNum} variants overlap with same start, \
                           cutting {Dist}",
                          r1.id, r2.idDistinction)
                routesByLicNum.[r2.id].Remove(r2) |> ignore
                routesByLicNum.[r2.id].Add(
                    {r2 with
                        timetableValidFrom =
                            r1.timetableValidTo + Period.FromDays(1)})
        // Ranges end at the same day, cut one
        else if r1.timetableValidTo = r2.timetableValidTo
             && r1.timetableValidFrom > r2.timetableValidFrom then
            if r2Priority then
                Log.Debug("Route {LicNum} variants overlap with same end, \
                           removing {Dist}",
                          r1.id, r1.idDistinction)
                this.deleteRoute(r1)
            else
                Log.Debug("Route {LicNum} variants overlap with same end, \
                           cutting {Dist}",
                          r1.id, r2.idDistinction)
                routesByLicNum.[r2.id].Remove(r2) |> ignore
                routesByLicNum.[r2.id].Add(
                    {r2 with
                        timetableValidTo =
                            r1.timetableValidFrom - Period.FromDays(1)})
        // r2 is a later variant, cut part of old route if not priority
        else if r1.timetableValidFrom < r2.timetableValidFrom
             && r1.timetableValidTo < r2.timetableValidTo then
            if not r1Priority then
                Log.Debug(
                    "Route {LicNum} variants overlap, cutting {Dist}",
                    r1.id, r1.idDistinction)
                routesByLicNum.[r1.id].Remove(r1) |> ignore
                routesByLicNum.[r1.id].Add(
                    {r1 with
                        timetableValidTo =
                            r2.timetableValidFrom - Period.FromDays(1)})
            else
                Log.Debug(
                    "Route {LicNum} variants overlap, cutting {Dist}, \
                     since {Dist2} has priority",
                    r1.id, r2.idDistinction, r1.idDistinction)
                routesByLicNum.[r2.id].Remove(r2) |> ignore
                routesByLicNum.[r2.id].Add(
                    {r2 with
                        timetableValidFrom =
                            r1.timetableValidTo + Period.FromDays(1)})
        // Validity ranges overlap and r2 is inside r1, duplicate r1
        // (if not priority)
        else if r1.timetableValidFrom < r2.timetableValidFrom
             && r1.timetableValidTo > r2.timetableValidTo then
            if r1Priority then
                Log.Debug(
                    "Route {LicNum} variants overlap and {Dist} is contained \
                     in {Dist2}, removing the former, since the latter has \
                     priority",
                    r1.id, r2.idDistinction, r1.idDistinction)
                this.deleteRoute(r2)
            else
                Log.Debug(
                    "Route {LicNum} variants overlap and {Dist} is contained \
                     in {Dist2}, splitting the latter",
                    r1.id, r2.idDistinction, r1.idDistinction)
                // Adjust r1's validity for first chunk of split
                routesByLicNum.[r1.id].Remove(r1) |> ignore
                routesByLicNum.[r1.id].Add(
                    {r1 with
                        timetableValidTo =
                            r2.timetableValidFrom - Period.FromDays(1)})
                // Duplicate of r1 for second chunk of split
                this.copyRoute(
                    {r1 with
                        timetableValidFrom =
                            r2.timetableValidTo + Period.FromDays(1)})
        // Resolve other overlaps by symmetry (simple overlap but r2 is first,
        // r1 is inside r2, same start/end date but r2 is shorter)
        else this.resolveOneRouteOverlap(r2, r1)

    member this.resolveRouteOverlaps() =
        for id in routesByLicNum.Keys do
            let handled = HashSet()
            // Pointer to mutable state
            let rsNow = routesByLicNum.[id]
            let unhandled () =
                rsNow
                |> Seq.tryFind (fun r ->
                    not <| handled.Contains(r.idDistinction))
            while unhandled () |> Option.isSome do
                let r2 = unhandled () |> Option.get
                handled.Add(r2.idDistinction) |> ignore
                let r1s =
                    rsNow
                    |> Seq.filter (fun r ->
                        r.idDistinction < r2.idDistinction)
                    |> Seq.sortBy (fun r -> r.idDistinction)
                    |> Seq.toArray
                for r1 in r1s do
                    // Take the newest version of r2
                    rsNow
                    |> Seq.tryFind (fun r ->
                        r.idDistinction = r2.idDistinction)
                    |> Option.iter (fun r2now ->
                        this.resolveOneRouteOverlap(r1, r2now))

    member this.beginAdd(batch: JdfBatch) =
        let attributeRefIdMap = Dictionary<int, int>()
        let existingAttributeRefsByValue =
            Dictionary<Attribute * string option, int>(attributeRefsByValue)
        for ar in batch.attributeRefs do
            match existingAttributeRefsByValue.TryGetValue((ar.value, ar.reserved1)) with
            | true, existingId -> attributeRefIdMap.[ar.attributeId] <- existingId
            | false, _ ->
                lastAttributeRefId <- lastAttributeRefId + 1
                let added = { ar with attributeId = lastAttributeRefId }
                attributeRefs.Add(added)
                attributeRefsByValue.[(added.value, added.reserved1)] <- added.attributeId
                attributeRefIdMap.[ar.attributeId] <- added.attributeId
        let mapAttributes attributes =
            let mapped =
                attributes
                |> Array.map (Option.map (fun id -> attributeRefIdMap.[id]))
            match attributeArrays.TryGetValue(mapped) with
            | true, interned -> interned
            | false, _ ->
                attributeArrays.Add(mapped, mapped)
                mapped

        let batchLocationsByStop = Dictionary<int64, StopLocation>()
        for location in batch.stopLocations do
            batchLocationsByStop.[location.stopId] <- location
        let stopIdMap = Dictionary<int64, int64>()
        let preferredIncomingLocationSources = HashSet<int64>()

        for sourceStop in batch.stops do
            let mappedStop = { sourceStop with attributes = mapAttributes sourceStop.attributes }
            let sourceLocation =
                match batchLocationsByStop.TryGetValue(sourceStop.id) with
                | true, location -> Some location
                | false, _ -> None
            match stopMergeStrategy with
            | MergeStopsById ->
                match stopsByIds.TryGetValue(sourceStop.id) with
                | true, existingId -> stopIdMap.[sourceStop.id] <- existingId
                | false, _ ->
                    stopsByIds.[sourceStop.id] <- sourceStop.id
                    stopIndexesById.[sourceStop.id] <- stops.Count
                    stops.Add(mappedStop)
                    stopIdMap.[sourceStop.id] <- sourceStop.id
            | MergeStopsByName ->
                match stopReconciler.FindMatch(sourceStop, sourceLocation) with
                | Choice1Of2 candidate ->
                    let stopIndex = stopIndexesById.[candidate.stopId]
                    let current = stops.[stopIndex]
                    let incomingPreferred = isCanonicalNamePreferred mappedStop current
                    let preferred = if incomingPreferred then mappedStop else current
                    let other = if incomingPreferred then current else mappedStop
                    stops.[stopIndex] <- {
                        preferred with
                            id = candidate.stopId
                            regionId = preferred.regionId |> Option.orElse other.regionId
                            country = preferred.country |> Option.orElse other.country
                            attributes = mergeAttributes candidate.stopId current.attributes mappedStop.attributes
                    }
                    if incomingPreferred then
                        preferredIncomingLocationSources.Add(sourceStop.id) |> ignore
                    stopIdMap.[sourceStop.id] <- candidate.stopId
                    stopReconciler.AddAlias(candidate.stopId, sourceStop, sourceLocation)
                    let aliasName = stopDisplayName sourceStop
                    let canonicalName = stopDisplayName stops.[stopIndex]
                    if aliasName <> canonicalName then
                        Log.Information(
                            "Merged stop {AliasName} into {CanonicalName} ({MatchKind}, "
                            + "Levenshtein {Levenshtein:F3}, Dice {Dice:F3}, distance {Distance})",
                            aliasName,
                            canonicalName,
                            candidate.kind,
                            candidate.levenshteinSimilarity,
                            candidate.tokenDiceSimilarity,
                            candidate.distance)
                | Choice2Of2 candidates ->
                    if candidates.Length > 0 then
                        let candidateDetails =
                            candidates
                            |> Array.map (fun candidate ->
                                let existing = stops.[stopIndexesById.[candidate.stopId]]
                                $"{stopDisplayName existing} "
                                + $"[{candidate.kind}; distance={candidate.distance}]" )
                        Log.Warning(
                            "Ambiguous stop reconciliation for {StopName}; candidates: {Candidates}",
                            stopDisplayName sourceStop,
                            candidateDetails)
                    lastStopId <- lastStopId + 1L
                    let added = { mappedStop with id = lastStopId }
                    stopIndexesById.[added.id] <- stops.Count
                    stops.Add(added)
                    stopIdMap.[sourceStop.id] <- added.id
                    stopReconciler.AddAlias(added.id, sourceStop, sourceLocation)
        let batchLocationSources = Dictionary<int64, string>()
        for value in batch.stopLocationSources do
            batchLocationSources.[value.stopId] <- value.source
        for sl in batch.stopLocations do
            let stopId = stopIdMap.[sl.stopId]
            let hasOldSl, oldSl = stopLocationsByStop.TryGetValue(stopId)

            // Check if old location isn't too far
            if hasOldSl
               && oldSl.precision = StopPrecise
               && sl.precision = StopPrecise
               && (oldSl.lat <> sl.lat || oldSl.lon <> sl.lon)
               && locationDistance sl oldSl > locationDistanceThresh then
                Log.Warning("Stop location for {StopId} in new batch is too \
                             far from existing location: {Lat}, {Lon}",
                            stopId, sl.lat, sl.lon)

            let precisionRank = function
                | StopPrecise -> 0
                | Estimated -> 1

            // Either this is a new location or a precision upgrade.
            if not hasOldSl
               || precisionRank sl.precision < precisionRank oldSl.precision
               || (precisionRank sl.precision = precisionRank oldSl.precision
                   && preferredIncomingLocationSources.Contains(sl.stopId))
            then
                stopLocationsByStop.[stopId] <- { sl with stopId = stopId }
                match batchLocationSources.TryGetValue(sl.stopId) with
                | true, source -> stopLocationSourcesByStop.[stopId] <- source
                | false, _ -> stopLocationSourcesByStop.Remove(stopId) |> ignore

        let stopPostsToAdd =
            batch.stopPosts
            |> Seq.filter (fun sp ->
                not <| stopPostsSet.Contains((stopIdMap.[sp.stopId], sp.stopPostId)))
            |> Seq.map (fun sp -> { sp with stopId = stopIdMap.[sp.stopId] })
        stopPosts.AddRange(stopPostsToAdd)
        for sp in stopPostsToAdd do
            stopPostsSet.Add((sp.stopId, sp.stopPostId)) |> ignore

        let existingAgenciesMap =
            batch.agencies
            |> Seq.map (fun a ->
                a,
                agenciesByIco.[a.id]
                |> Seq.tryFind (fun a2 ->
                    a = {a2 with idDistinction = a.idDistinction}))
            |> Seq.cache
        let agenciesToAdd =
            existingAgenciesMap
            |> Seq.filter (fun (_, e) -> Option.isNone e)
            |> Seq.map fst
            |> Seq.cache
        let agenciesToAddNewId =
            agenciesToAdd
            |> Seq.map (fun a ->
                let other: Agency ResizeArray = agenciesByIco.[a.id]
                let subId =
                    if other |> Seq.isEmpty then 1
                    else (other
                          |> Seq.map (fun a -> a.idDistinction)
                          |> Seq.max) + 1
                let ani = { a with idDistinction = subId }
                // We need to add it right away, in case the input has multiple
                // agencies of the same ID
                agenciesByIco.[a.id].Add(ani)
                ani)
            |> Seq.toArray
        let agencyIdMap =
            Seq.concat [
                Seq.zip agenciesToAdd agenciesToAddNewId
                |> Seq.map (fun (a, ani) ->
                    (a.id, a.idDistinction), (ani.id, ani.idDistinction))

                existingAgenciesMap
                |> Seq.choose (fun (a, eo) ->
                    eo |> Option.map (fun e ->
                        (a.id, a.idDistinction), (e.id, e.idDistinction)))
            ]
            |> Map

        // For later merging steps
        let routesMap = Dictionary<string * int, Route>()
        for route in batch.routes do
            routesMap.[(route.id, route.idDistinction)] <- route
        // We don't resolve validity overlaps here and leave that for a
        // post-processing phase
        let newRoutes =
            batch.routes
            |> Array.map (fun r ->
                let other = routesByLicNum.[r.id]
                let aid, aidd = agencyIdMap.[(r.agencyId, r.agencyDistinction)]
                {r with
                    idDistinction = (Seq.length other) + 1
                    agencyId = aid
                    agencyDistinction = aidd})
        for r in newRoutes do
            routesByLicNum.[r.id].Add(r)

            batchDateByRoute.[(r.id, r.idDistinction)] <-
                batch.version.creationDate
        let routeIdMap = Dictionary<string * int, string * int>()
        for source, mapped in Seq.zip batch.routes newRoutes do
            routeIdMap.[(source.id, source.idDistinction)] <-
                (mapped.id, mapped.idDistinction)

        batch.routeIntegrations
        |> Seq.map (fun ri ->
            let rid, ridd = routeIdMap.[(ri.routeId, ri.routeDistinction)]
            { ri with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun ri -> ri.routeId, ri.routeDistinction)
        |> Seq.iter (fun (k, v) -> routeIntegrationsByRoute.[k] <- v)

        batch.routeStops
        |> Seq.map (fun rs ->
            let rid, ridd = routeIdMap.[(rs.routeId, rs.routeDistinction)]
            { rs with
                routeId = rid
                routeDistinction = ridd
                stopId = stopIdMap.[rs.stopId]
                attributes = mapAttributes rs.attributes
            })
        |> Seq.groupBy (fun rs -> rs.routeId, rs.routeDistinction)
        |> Seq.iter (fun (k, v) -> routeStopsByRoute.[k] <- v)

        let newTripGroups =
            batch.tripGroups
            |> Seq.map (fun tg ->
                lastTripGroupId <- lastTripGroupId + 1
                { tg with
                    id = lastTripGroupId
                })
        tripGroups.AddRange(newTripGroups)
        let tripGroupIdMap =
            Seq.zip batch.tripGroups newTripGroups
            |> Seq.map (fun (tg, tgni) -> tg.id, tgni.id)
            |> Map

        batch.trips
        |> Seq.map (fun t ->
            let rid, ridd = routeIdMap.[(t.routeId, t.routeDistinction)]
            let route = routesMap.[(t.routeId, t.routeDistinction)]
            { t with
                routeId = rid
                routeDistinction = ridd
                tripGroupId =
                    if route.grouped
                    then Some tripGroupIdMap.[Option.get t.tripGroupId]
                    else None
                attributes = mapAttributes t.attributes
            })
        |> Seq.groupBy (fun t -> t.routeId, t.routeDistinction)
        |> Seq.iter (fun (k, v) -> tripsByRoute.[k] <- v)

        let tripStopAttributes =
            Dictionary<int option array, int option array>(HashIdentity.Structural)
        // Populate the structural interning dictionaries in source order
        // before workers perform read-only lookups. This keeps allocation and
        // dictionary insertion deterministic while allowing the dominant
        // relation's remapping to use the aggressive CLI worker ceiling.
        for source in batch.tripStops do
            if not (tripStopAttributes.ContainsKey(source.attributes)) then
                tripStopAttributes.Add(source.attributes, mapAttributes source.attributes)
        let mappedTripStop (ts: TripStop) =
            let rid, ridd = routeIdMap.[(ts.routeId, ts.routeDistinction)]
            { ts with
                routeId = rid
                routeDistinction = ridd
                stopId = stopIdMap.[ts.stopId]
                attributes = tripStopAttributes.[ts.attributes]
            }
        let tripStopRegistration =
            match tripStopSpool with
            | Some spool ->
                spool.BeginMappedBatch(batch.tripStops, tripStopTransformWorkers, mappedTripStop)
            | None ->
                batch.tripStops
                |> mapParallelOrderedBatches tripStopTransformWorkers mappedTripStop
                |> Seq.groupBy (fun ts -> ts.routeId, ts.routeDistinction)
                |> Seq.iter (fun (k, v) -> tripStopsByRoute.[k] <- v)
                Threading.Tasks.Task.FromResult(fun () -> ())

        batch.routeInfo
        |> Seq.map (fun ri ->
            let rid, ridd = routeIdMap.[(ri.routeId, ri.routeDistinction)]
            { ri with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun ri -> ri.routeId, ri.routeDistinction)
        |> Seq.iter (fun (k, v) -> routeInfoByRoute.[k] <- v)

        batch.serviceNotes
        |> Seq.map (fun sn ->
            let rid, ridd = routeIdMap.[(sn.routeId, sn.routeDistinction)]
            { sn with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun sn -> sn.routeId, sn.routeDistinction)
        |> Seq.iter (fun (k, v) -> serviceNotesByRoute.[k] <- v)

        batch.transfers
        |> Seq.map (fun t ->
            let rid, ridd = routeIdMap.[(t.routeId, t.routeDistinction)]
            { t with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun t -> t.routeId, t.routeDistinction)
        |> Seq.iter (fun (k, v) -> transfersByRoute.[k] <- v)

        batch.agencyAlternations
        |> Seq.map (fun aa ->
            let rid, ridd = routeIdMap.[(aa.routeId, aa.routeDistinction)]
            let aid, aidd = agencyIdMap.[(aa.agencyId, aa.agencyDistinction)]
            { aa with
                routeId = rid
                routeDistinction = ridd
                agencyId = aid
                agencyDistinction = aidd
                attributes = mapAttributes aa.attributes
            })
        |> Seq.groupBy (fun aa -> aa.routeId, aa.routeDistinction)
        |> Seq.iter (fun (k, v) -> agencyAlternationsByRoute.[k] <- v)

        batch.alternateRouteNames
        |> Seq.map (fun arn ->
            let rid, ridd = routeIdMap.[(arn.routeId, arn.routeDistinction)]
            { arn with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun arn -> arn.routeId, arn.routeDistinction)
        |> Seq.iter (fun (k, v) -> alternateRouteNamesByRoute.[k] <- v)

        batch.reservationOptions
        |> Seq.map (fun ro ->
            let rid, ridd = routeIdMap.[(ro.routeId, ro.routeDistinction)]
            { ro with
                routeId = rid
                routeDistinction = ridd
            })
        |> Seq.groupBy (fun ro -> ro.routeId, ro.routeDistinction)
        |> Seq.iter (fun (k, v) -> reservationOptionsByRoute.[k] <- v)

        tripStopRegistration

    member this.add(batch: JdfBatch) =
        let register = this.beginAdd(batch).GetAwaiter().GetResult()
        register ()

    interface IDisposable with
        member _.Dispose() =
            tripStopSpool
            |> Option.iter (fun spool -> (spool :> IDisposable).Dispose())
