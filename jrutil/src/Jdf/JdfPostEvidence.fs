// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

module JrUtil.JdfPostEvidence

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text
open System.Threading
open System.Threading.Tasks

open JrUtil.GeoData.Osm
open JrUtil.JdfPostInference

type PostEvidenceObservation = {
    stopId:int64;routePointId:string;observationId:string;sourceKind:string
    sourceObjectId:string option;observedAt:string option;latitude:float;longitude:float
    supportWeight:float;rawTags:string;explicitModes:string;deniedModes:string;lifecycle:string
}

type PostEvidenceRoutePoint = {
    stopId:int64;routePointId:string;representativeObservationId:string
    observationIds:string array;latitude:float;longitude:float
    hasCurrentLifecycle:bool;hasObsoleteLifecycle:bool;sourceSupportWeight:float
    explicitModes:string array;deniedModes:string array
}

[<Struct>]
type PostEvidenceContextKey = {
    stopId:int64;mode:string;lineId:string;routeDistinction:int
    direction:int;patternHash:string;patternPosition:int;sameStopBlockRole:string
    authoredPostKey:string option
}

type PostEvidenceContext = {
    ordinal:int;contextId:string;key:PostEvidenceContextKey;movementFamilyId:string
    previousStopId:int64 option;nextStopId:int64 option
    assignmentKind:string;sameStopBlockId:string option
}

type PostEvidenceCorridorVariant = {
    ordinal:int;contextId:string;stopId:int64;variant:RoutedCorridorVariant
}

type PostEvidenceRoutePointAttachment = {
    ordinal:int;contextId:string;stopId:int64;routePointId:string
    attachment:RoutePointVariantAttachment
}

type CaptureCoordinate = { longitude:float;latitude:float }

type CaptureWorkRow = {
    context:PostEvidenceContext
    previousCoordinates:CaptureCoordinate array
    nextCoordinates:CaptureCoordinate array
    points:PostEvidenceRoutePoint array
}

type CaptureRoutingWorkRow = {
    ordinal:int
    routingKey:string
    work:CaptureWorkRow
}

type RoutedCaptureRow = {
    ordinal:int
    routingKey:string
    work:CaptureWorkRow
    routed:RoutedContextEvidence
}

let private createReplayStore<'T> memoryBudgetBytes temporaryDirectory rows =
    ReplayableRowStore<'T>.Create(memoryBudgetBytes,temporaryDirectory,rows)

type private ProjectedRowStore<'T>(count:int64,rows:unit -> seq<'T>) =
    interface IReplayableRowStore<'T> with
        member _.ReadRows()=rows()
        member _.Count=count
        member _.CurrentSpillBytes=0L
        member _.PeakSpillBytes=0L
        member _.Dispose()=()

type private OwnershipGuard(stores:IDisposable array) =
    let mutable transferred=false
    member _.Transfer()=transferred<-true
    interface IDisposable with
        member _.Dispose() =
            if not transferred then
                for store in stores do
                    try store.Dispose() with _ -> ()

type PostEvidenceCaptureOptions = {
    maximumWorkers:int
    memoryBudgetBytes:int64
    preflight:PostEvidenceCaptureUpperBounds -> unit
    progress:string -> int64 -> int64 option -> string option -> unit
}

and PostEvidenceCaptureUpperBounds = {
    observationCount:int64
    routePointCount:int64
    sourceContextCount:int64
    contextCount:int64
    routingEvidenceCount:int64
    corridorVariantCount:int64
    routePointEvidenceCount:int64
}

let defaultCaptureOptions = {
    maximumWorkers=1;memoryBudgetBytes=Int64.MaxValue
    preflight=ignore;progress=fun _ _ _ _ -> ()
}

/// Transient rows produced by the router.  This value is deliberately not an
/// evaluator input: live evaluation must first publish and reopen the
/// validated persisted representation through JdfPostEvidenceStore.
type CapturedPostEvidence(
        observations:IReplayableRowStore<PostEvidenceObservation>,
        routePoints:IReplayableRowStore<PostEvidenceRoutePoint>,
        contexts:IReplayableRowStore<PostEvidenceContext>,
        corridorVariants:IReplayableRowStore<PostEvidenceCorridorVariant>,
        routePointEvidence:IReplayableRowStore<PostEvidenceRoutePointAttachment>,
        transientDependencies:IDisposable array,
        transientCurrentSpillBytes:unit -> int64,
        transientPeakSpillBytes:int64) =
    member _.observations=observations
    member _.routePoints=routePoints
    member _.contexts=contexts
    member _.corridorVariants=corridorVariants
    member _.routePointEvidence=routePointEvidence
    member _.CurrentSpillBytes =
        observations.CurrentSpillBytes+routePoints.CurrentSpillBytes+
        contexts.CurrentSpillBytes+corridorVariants.CurrentSpillBytes+
        routePointEvidence.CurrentSpillBytes+transientCurrentSpillBytes()
    member _.PeakSpillBytes =
        transientPeakSpillBytes+observations.PeakSpillBytes+routePoints.PeakSpillBytes+
        contexts.PeakSpillBytes+corridorVariants.PeakSpillBytes+
        routePointEvidence.PeakSpillBytes
    interface IDisposable with
        member _.Dispose() =
            let mutable firstFailure:exn option=None
            for store in [|observations :> IDisposable;routePoints :> IDisposable
                           contexts :> IDisposable;corridorVariants :> IDisposable
                           routePointEvidence :> IDisposable
                           yield! transientDependencies|] do
                try store.Dispose()
                with error -> if firstFailure.IsNone then firstFailure<-Some error
            firstFailure |> Option.iter raise

let validateCapturedEvidence (store:CapturedPostEvidence) =
    let uniqueOrdered name key rows =
        let mutable previous=None
        for row in rows do
            let current=key row
            match previous with
            | Some value when compare value current>=0 ->
                invalidArg "store" (name+" is not in unique canonical order")
            | _ -> previous<-Some current
    uniqueOrdered "observations"
        (fun (value:PostEvidenceObservation) -> value.stopId,value.observationId)
        (store.observations.ReadRows())
    uniqueOrdered "route points"
        (fun (value:PostEvidenceRoutePoint) -> value.stopId,value.routePointId)
        (store.routePoints.ReadRows())
    uniqueOrdered "contexts" (fun (value:PostEvidenceContext) -> value.ordinal)
        (store.contexts.ReadRows())
    uniqueOrdered "corridor variants"
        (fun (value:PostEvidenceCorridorVariant) -> value.ordinal,value.variant.variantRank)
        (store.corridorVariants.ReadRows())
    uniqueOrdered "route-point attachments"
        (fun (value:PostEvidenceRoutePointAttachment) ->
            value.ordinal,value.attachment.variantRank,value.routePointId)
        (store.routePointEvidence.ReadRows())
    let pointIds=store.routePoints.ReadRows() |> Seq.map _.routePointId |> Set.ofSeq
    for observation in store.observations.ReadRows() do
        if not(pointIds.Contains observation.routePointId) then
            invalidArg "store" "An observation references an unknown route point"
        if not(Double.IsFinite observation.latitude && Double.IsFinite observation.longitude
               && Double.IsFinite observation.supportWeight) then
            invalidArg "store" "Observation evidence contains a non-finite value"
    for attachment in store.routePointEvidence.ReadRows() do
        let finite value=value |> Option.forall Double.IsFinite
        let fact=attachment.attachment
        if not(finite fact.routedExcessMetres && finite fact.snapFraction
               && finite fact.snapProjectedLongitude && finite fact.snapProjectedLatitude
               && finite fact.snapDistanceMetres && finite fact.corridorDistanceMetres
               && finite fact.signedLateralOffsetMetres && finite fact.corridorHeadingDegrees
               && finite fact.attachmentHeadingDegrees && finite fact.headingDifferenceDegrees
               && finite fact.proximityDistanceMetres) then
            invalidArg "store" "Route-point attachment contains a non-finite value"
    store

let private stableId prefix (parts:string array) =
    let payload=String.Join("|",parts)
    prefix+(SHA256.HashData(Encoding.UTF8.GetBytes(payload))
            |> Convert.ToHexString |> fun value -> value.ToLowerInvariant())

let routePointId stopId (latitude:decimal) (longitude:decimal) =
    stableId "route-point:" [|
        string stopId
        latitude.ToString(Globalization.CultureInfo.InvariantCulture)
        longitude.ToString(Globalization.CultureInfo.InvariantCulture) |]

let private completePatternHash (calls:JdfModel.TripStop array) =
    calls |> Array.map(fun call -> string call.stopId) |> String.concat ","
    |> fun value -> stableId "" [|value|]

let private authoredPostKey (call:JdfModel.TripStop) =
    let trimmed value = value |> Option.map(fun (text:string) -> text.Trim()) |> Option.filter(String.IsNullOrWhiteSpace >> not)
    match call.stopPostId,trimmed call.stopPostNum with
    | Some value,_ -> Some(sprintf "id:%O" value)
    | None,Some value -> Some(sprintf "num:%s" value)
    | _ -> None

let private callIsUsable (call:JdfModel.TripStop) =
    match call.departureTime,call.arrivalTime with
    | Some JdfModel.Passing,_ | Some JdfModel.NotPassing,_ -> false
    | None,None -> false
    | _ -> true

let private modeCode = function
    | JdfModel.Tram -> "E" | JdfModel.Trolleybus -> "T" | _ -> "A"

let private splitModes (values:string seq) =
    values
    |> Seq.collect(fun value -> value.Split([|';';',';' '|],StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map(fun value -> value.Trim().ToUpperInvariant())
    |> Seq.filter(String.IsNullOrWhiteSpace >> not) |> Seq.distinct |> Seq.sort |> Seq.toArray

let private distanceSquared (leftLat,leftLon) (rightLat,rightLon) =
    let latitude=(leftLat+rightLat)*Math.PI/360.0
    let dx=(leftLon-rightLon)*111_320.0*Math.Cos(latitude)
    let dy=(leftLat-rightLat)*110_540.0
    dx*dx+dy*dy

let private medoidCoordinate (points:PostEvidenceRoutePoint array) =
    points
    |> Array.minBy(fun point ->
        points |> Array.sumBy(fun other ->
            distanceSquared(point.latitude,point.longitude)(other.latitude,other.longitude)),
        point.routePointId)
    |> fun point -> struct(point.longitude,point.latitude)

let captureToStoreWithCancellation (cancellationToken:CancellationToken)
                                   (options:PostEvidenceCaptureOptions)
                                   (graph:PackedRoutingGraph)(batch:JdfModel.JdfBatch) =
    cancellationToken.ThrowIfCancellationRequested()
    if options.maximumWorkers<1 then invalidArg "options" "maximumWorkers must be positive"
    if options.memoryBudgetBytes<1L then invalidArg "options" "memoryBudgetBytes must be positive"
    let temporaryDirectory=Path.Combine(Path.GetTempPath(),"jrutil-post-evidence-capture")
    let storeBudget=max 1L (options.memoryBudgetBytes/6L)
    let observationRows =
        batch.postCandidateEvidence
        |> Seq.map(fun value ->
            { stopId=value.stopId;routePointId=routePointId value.stopId value.lat value.lon
              observationId=value.observationId;sourceKind=value.sourceKind
              sourceObjectId=value.sourceObjectId;observedAt=value.observedAt
              latitude=float value.lat;longitude=float value.lon;supportWeight=float value.supportWeight
              rawTags=value.rawTags;explicitModes=value.explicitModes;deniedModes=value.deniedModes
              lifecycle=value.lifecycle })
    let observationStore,routePointStore,routePointPreparationPeak =
        let mutable observationStore:IReplayableRowStore<PostEvidenceObservation> option=None
        try
            let observations=ReplayableRowStore<PostEvidenceObservation>.CreateSorted(
                storeBudget,temporaryDirectory,
                (fun value -> value.stopId,value.observationId),observationRows)
            observationStore<-Some observations
            use members=ReplayableRowStore<PostEvidenceObservation>.CreateSorted(
                storeBudget,temporaryDirectory,
                (fun value ->
                    value.stopId,value.routePointId,value.latitude,value.longitude,value.observationId),
                observations.ReadRows())
            let routePointRows =
                members.ReadRows()
                |> Utils.groupAdjacentBy(fun value ->
                    value.stopId,value.routePointId,value.latitude,value.longitude)
                |> Seq.map(fun ((stopId,id,latitude,longitude),members) ->
                    let lifecycle=members |> Array.map(fun value -> value.lifecycle.Trim().ToLowerInvariant())
                    { stopId=stopId;routePointId=id
                      representativeObservationId=members |> Array.minBy _.observationId |> _.observationId
                      observationIds=members |> Array.map _.observationId |> Array.distinct |> Array.sort
                      latitude=latitude;longitude=longitude
                      hasCurrentLifecycle=lifecycle |> Array.exists(fun value -> value="" || value="active")
                      hasObsoleteLifecycle=lifecycle |> Array.exists(fun value ->
                          value="abandoned" || value="disused" || value="construction")
                      sourceSupportWeight=members |> Array.sumBy _.supportWeight
                      explicitModes=members |> Seq.map _.explicitModes |> splitModes
                      deniedModes=members |> Seq.map _.deniedModes |> splitModes })
            let routePoints=ReplayableRowStore<PostEvidenceRoutePoint>.CreateSorted(
                storeBudget,temporaryDirectory,
                (fun value -> value.stopId,value.routePointId),routePointRows)
            observations,routePoints,members.PeakSpillBytes
        with _ ->
            observationStore |> Option.iter _.Dispose()
            reraise()
    use preparationOwnership=
        new OwnershipGuard([|observationStore :> IDisposable;routePointStore :> IDisposable|])
    let pointsByStop=
        routePointStore.ReadRows()
        |> Utils.groupAdjacentBy _.stopId
        |> Map.ofSeq
    let preciseLocations=
        batch.stopLocations |> Seq.filter(fun value -> value.precision=JdfModel.StopPrecise)
        |> Seq.groupBy _.stopId
        |> Seq.map(fun (stopId,values) ->
            let value=values |> Seq.sortBy(fun item -> item.lat,item.lon) |> Seq.head
            stopId,struct(float value.lon,float value.lat)) |> Map.ofSeq
    let anchors=
        pointsByStop
        |> Map.fold(fun state stopId points -> Map.add stopId (medoidCoordinate points) state) preciseLocations
    let modes=batch.routes |> Seq.map(fun route -> (route.id,route.idDistinction),route.transportMode) |> Map.ofSeq
    let groups=batch.tripStops |> Utils.groupAdjacentBy(fun call -> call.routeId,call.routeDistinction,call.tripId)
    let seen=HashSet<string>()
    let patternRows = seq {
        for ((routeId,distinction,_),calls) in groups do
            let mode=modes.[routeId,distinction]
            if mode=JdfModel.Bus || mode=JdfModel.Trolleybus || mode=JdfModel.Tram then
                let ordered=calls |> Array.filter callIsUsable
                                  |> Array.sortBy(fun call ->
                                      call.routeStopId*(if Jdf.tripIsReverse call.tripId then -1L else 1L))
                let pattern=completePatternHash ordered
                let direction=if ordered.Length>0 && Jdf.tripIsReverse ordered.[0].tripId then 1 else 0
                let authored=ordered |> Array.map(authoredPostKey >> Option.defaultValue "") |> String.concat ";"
                let identity=sprintf "%s|%i|%i|%s|%s" routeId distinction direction pattern authored
                if seen.Add(identity) then
                    yield routeId,distinction,mode,ordered,pattern,direction }
    let workRows = seq {
        for routeId,distinction,mode,ordered,pattern,direction in patternRows do
            let mutable start=0
            while start<ordered.Length do
                let stopId=ordered.[start].stopId
                let mutable finish=start+1
                while finish<ordered.Length && ordered.[finish].stopId=stopId do finish<-finish+1
                match pointsByStop |> Map.tryFind stopId with
                | Some points when points.Length>=2 ->
                    let previous=
                        seq {start-1 .. -1 .. 0} |> Seq.tryPick(fun index ->
                            if ordered.[index].stopId=stopId then None
                            else anchors |> Map.tryFind ordered.[index].stopId
                                 |> Option.map(fun coordinate -> ordered.[index].stopId,coordinate))
                    let next=
                        seq {finish .. ordered.Length-1} |> Seq.tryPick(fun index ->
                            if ordered.[index].stopId=stopId then None
                            else anchors |> Map.tryFind ordered.[index].stopId
                                 |> Option.map(fun coordinate -> ordered.[index].stopId,coordinate))
                    let pointCoordinates=
                        points |> Array.map(fun point ->
                            {longitude=point.longitude;latitude=point.latitude})
                    let blockId=
                        if finish-start>1 then
                            Some(stableId "same-stop-block:"
                                [|routeId;string distinction;pattern;string start;string finish|])
                        else None
                    for index=start to finish-1 do
                        let role=
                            if finish-start=1 then "through"
                            elif index=start then "incoming"
                            elif index=finish-1 then "outgoing"
                            else "interior"
                        let previousId,previousCoordinates =
                            if role="outgoing" || role="interior" then None,pointCoordinates
                            else
                                previous |> Option.map fst,
                                previous
                                |> Option.map(fun (_,struct(longitude,latitude)) ->
                                    [|{longitude=longitude;latitude=latitude}|])
                                |> Option.defaultValue pointCoordinates
                        let nextId,nextCoordinates =
                            if role="incoming" || role="interior" then None,pointCoordinates
                            else
                                next |> Option.map fst,
                                next
                                |> Option.map(fun (_,struct(longitude,latitude)) ->
                                    [|{longitude=longitude;latitude=latitude}|])
                                |> Option.defaultValue pointCoordinates
                        let authored=authoredPostKey ordered.[index]
                        let key={stopId=stopId;mode=modeCode mode;lineId=routeId
                                 routeDistinction=distinction;direction=direction
                                 patternHash=pattern;patternPosition=index
                                 sameStopBlockRole=role;authoredPostKey=authored}
                        let contextId=
                            stableId "context:" [|
                                string stopId;modeCode mode;routeId;string distinction
                                string direction;pattern;string index;role
                                authored |> Option.defaultValue "" |]
                        yield {
                            context={ordinal = -1;contextId=contextId;key=key;movementFamilyId=""
                                     previousStopId=previousId;nextStopId=nextId
                                     assignmentKind=(if authored.IsSome then "authored" else "unlabelled")
                                     sameStopBlockId=blockId}
                            previousCoordinates=previousCoordinates
                            nextCoordinates=nextCoordinates
                            points=points }
                | _ -> ()
                start<-finish }
    let workKey (value:CaptureWorkRow) =
        value.context.key.stopId,value.context.key.mode,value.context.key.lineId,
        value.context.key.routeDistinction,value.context.key.direction,
        value.context.key.patternHash,value.context.key.patternPosition,
        value.context.key.sameStopBlockRole,value.context.key.authoredPostKey
    let distinctAdjacentWork (rows:seq<CaptureWorkRow>) = seq {
        use enumerator=rows.GetEnumerator()
        if enumerator.MoveNext() then
            let mutable previous=enumerator.Current
            let mutable previousKey=workKey previous
            while enumerator.MoveNext() do
                let current=enumerator.Current
                let currentKey=workKey current
                if currentKey=previousKey then
                    if current<>previous then
                        invalidArg "batch"
                            "Equivalent post-inference contexts disagree on routing inputs"
                else
                    yield previous
                    previous<-current
                    previousKey<-currentKey
            yield previous }
    let orderedWork,sourceContextCount,workPreparationPeak =
        use sourceWork =
            ReplayableRowStore<CaptureWorkRow>.CreateSorted(
                storeBudget,temporaryDirectory,workKey,workRows)
        let sourceCount=sourceWork.Count
        let canonical =
            ReplayableRowStore<CaptureWorkRow>.Create(
                storeBudget,temporaryDirectory,
                sourceWork.ReadRows() |> distinctAdjacentWork)
        let peak=
            max sourceWork.PeakSpillBytes
                (sourceWork.CurrentSpillBytes+canonical.PeakSpillBytes)
        canonical,sourceCount,peak
    let orderedWork=orderedWork
    use orderedWorkOwnership=new OwnershipGuard([|orderedWork :> IDisposable|])
    let coordinateKey (value:CaptureCoordinate) =
        $"{BitConverter.DoubleToInt64Bits(value.longitude):x16}{BitConverter.DoubleToInt64Bits(value.latitude):x16}"
    let routingKey (value:CaptureWorkRow) =
        stableId "routing-evidence:" [|
            string value.context.key.stopId
            value.context.key.mode
            value.previousCoordinates |> Array.map coordinateKey |> String.concat ","
            value.points
            |> Array.map(fun point ->
                $"{BitConverter.DoubleToInt64Bits(point.longitude):x16}{BitConverter.DoubleToInt64Bits(point.latitude):x16}")
            |> String.concat ","
            value.nextCoordinates |> Array.map coordinateKey |> String.concat "," |]
    let routingWorkRows = seq {
        let mutable currentGroup:struct(int64*string) option=None
        let groupKeys=HashSet<string>(StringComparer.Ordinal)
        for value in orderedWork.ReadRows() do
            let group=struct(value.context.key.stopId,value.context.key.mode)
            if currentGroup<>Some group then
                currentGroup<-Some group
                groupKeys.Clear()
            let key=routingKey value
            if groupKeys.Add(key) then yield key,value }
    let routingWork =
        ReplayableRowStore<CaptureRoutingWorkRow>.Create(
            storeBudget,temporaryDirectory,
            routingWorkRows
            |> Seq.mapi(fun ordinal (key,work) ->
                {ordinal=ordinal;routingKey=key;work=work}))
    use routingWork=routingWork
    let contextCount=orderedWork.Count
    let routingEvidenceCount=routingWork.Count
    let workPreparationPeak=
        max workPreparationPeak
            (orderedWork.CurrentSpillBytes+routingWork.PeakSpillBytes)
    let maximumCorridorVariants=int64 JdfPostInference.CaptureMaximumCorridorVariants
    let maximumRoutePointEvidenceCount =
        orderedWork.ReadRows()
        |> Seq.sumBy(fun value -> int64 value.points.Length*maximumCorridorVariants)
    options.preflight {
        observationCount=observationStore.Count
        routePointCount=routePointStore.Count
        sourceContextCount=sourceContextCount
        contextCount=contextCount
        routingEvidenceCount=routingEvidenceCount
        corridorVariantCount=contextCount*maximumCorridorVariants
        routePointEvidenceCount=maximumRoutePointEvidenceCount
    }
    let maximumContextCharge =
        pointsByStop
        |> Seq.map(fun pair ->
            let points=pair.Value
            64L*1024L + int64 points.Length*int64 JdfPostInference.CaptureMaximumCorridorVariants*512L)
        |> Seq.append [1L]
        |> Seq.max
    let budgetWorkers=max 1 (int(min (int64 Int32.MaxValue) (options.memoryBudgetBytes/maximumContextCharge)))
    let admittedWorkers=min options.maximumWorkers budgetWorkers
    options.progress "capture-routing-admission" 0L (Some routingEvidenceCount)
        (Some $"workers={admittedWorkers}; routing_evidence={routingEvidenceCount}; logical_contexts={contextCount}; context_charge_bytes={maximumContextCharge}; budget_bytes={options.memoryBudgetBytes}")
    let originalMaximumWorkers=graph.MaximumWorkers
    let mutable routedStore:IReplayableRowStore<RoutedCaptureRow> option=None
    let owned=ResizeArray<IDisposable>()
    try
        use linkedCancellation=
            CancellationTokenSource.CreateLinkedTokenSource(cancellationToken)
        let workerBudget=max 1L (storeBudget/int64 admittedWorkers)
        let queues=
            Array.init admittedWorkers (fun _ ->
                new BlockingCollection<CaptureRoutingWorkRow>(1))
        let completeQueues () =
            for queue in queues do
                if not queue.IsAddingCompleted then queue.CompleteAdding()
        let mutable completed=0L
        let progressLock=obj()
        let route work =
            let points=work.points
            let coordinates=points |> Array.map(fun point -> struct(point.longitude,point.latitude))
            let previous=work.previousCoordinates |> Array.map(fun value -> struct(value.longitude,value.latitude))
            let next=work.nextCoordinates |> Array.map(fun value -> struct(value.longitude,value.latitude))
            let routingMode=
                match work.context.key.mode with
                | "E" -> TramRouting
                | "T" -> Trolleybus
                | _ -> RoadBus
            let result=graph.CaptureContextEvidence(routingMode,previous,coordinates,next)
            let count=Interlocked.Increment(&completed)
            if count%250L=0L then
                lock progressLock (fun () ->
                    options.progress "capture-routing-contexts" count
                        (Some routingEvidenceCount)
                        (Some $"logical_contexts={contextCount}"))
            result
        let workerTasks =
            queues
            |> Array.map(fun queue ->
                Task.Run(fun () ->
                    try
                        ReplayableRowStore<RoutedCaptureRow>.Create(
                            workerBudget,temporaryDirectory,seq {
                                for item in queue.GetConsumingEnumerable(linkedCancellation.Token) do
                                    yield {
                                        ordinal=item.ordinal
                                        routingKey=item.routingKey
                                        work=item.work
                                        routed=route item.work } })
                    with _ ->
                        linkedCancellation.Cancel()
                        reraise()))
        let mutable dispatchFailure:exn option=None
        try
            graph.MaximumWorkers<-1
            try
                try
                    for item in routingWork.ReadRows() do
                        linkedCancellation.Token.ThrowIfCancellationRequested()
                        queues.[item.ordinal%admittedWorkers].Add(item,linkedCancellation.Token)
                with error ->
                    dispatchFailure<-Some error
                    linkedCancellation.Cancel()
            finally
                completeQueues()
            let workerStores=Array.zeroCreate<IReplayableRowStore<RoutedCaptureRow>> admittedWorkers
            let mutable workerFailure:exn option=None
            for index=0 to workerTasks.Length-1 do
                try
                    workerStores.[index]<-workerTasks.[index].GetAwaiter().GetResult()
                with
                | error when workerFailure.IsNone -> workerFailure<-Some error
                | _ -> ()
            let failure=
                match workerFailure with
                | Some error -> Some error
                | None -> dispatchFailure
            match failure with
            | Some error ->
                for store in workerStores do
                    if not(isNull(box store)) then store.Dispose()
                raise error
            | None ->
                try
                    routedStore<-Some(ReplayableRowStore<RoutedCaptureRow>.MergeSorted(
                        storeBudget,temporaryDirectory,(fun row -> row.ordinal),workerStores))
                finally
                    for store in workerStores do store.Dispose()
        finally
            graph.MaximumWorkers<-originalMaximumWorkers
            completeQueues()
            for queue in queues do queue.Dispose()
    with _ ->
        for value in owned do value.Dispose()
        reraise()
    try
        let routed=routedStore.Value
        let joinedRows () = seq {
            use routedRows=routed.ReadRows().GetEnumerator()
            let cache=Dictionary<string,RoutedContextEvidence>(StringComparer.Ordinal)
            let mutable currentRouted =
                if routedRows.MoveNext() then Some routedRows.Current else None
            let mutable currentGroup:struct(int64*string) option=None
            let mutable ordinal=0
            for work in orderedWork.ReadRows() do
                let group=struct(work.context.key.stopId,work.context.key.mode)
                if currentGroup<>Some group then
                    currentGroup<-Some group
                    cache.Clear()
                    let mutable reading=true
                    while reading do
                        match currentRouted with
                        | Some row when
                                struct(row.work.context.key.stopId,row.work.context.key.mode)=group ->
                            cache.Add(row.routingKey,row.routed)
                            currentRouted<-
                                if routedRows.MoveNext() then Some routedRows.Current else None
                        | _ -> reading<-false
                let key=routingKey work
                match cache.TryGetValue key with
                | true,evidence ->
                    yield struct(ordinal,work,evidence)
                    ordinal<-ordinal+1
                | _ -> invalidOp "Routed evidence is missing a logical context geometry"
            if currentRouted.IsSome then
                invalidOp "Routed evidence contains an unused context geometry" }
        let withFamily ordinal (work:CaptureWorkRow) routed =
            let baseline=routed.variants.[0]
            let movementFamilyId=
                stableId "movement-family:" [|
                    sprintf "%i" work.context.key.stopId
                    work.context.key.mode
                    baseline.ingressThreadId |> Option.defaultValue ""
                    baseline.egressThreadId |> Option.defaultValue ""
                    work.context.key.sameStopBlockRole |]
            {work.context with ordinal=ordinal;movementFamilyId=movementFamilyId}
        let contextRows () =
            joinedRows()
            |> Seq.map(fun struct(ordinal,work,evidence) ->
                withFamily ordinal work evidence)
        let corridorRows () = seq {
            for struct(ordinal,work,evidence) in joinedRows() do
                for variant in evidence.variants do
                    yield {ordinal=ordinal;contextId=work.context.contextId;stopId=work.context.key.stopId
                           variant=variant} }
        let attachmentRows () = seq {
            for struct(ordinal,work,evidence) in joinedRows() do
                for variantIndex=0 to evidence.attachments.Length-1 do
                    for pointIndex=0 to work.points.Length-1 do
                        yield {ordinal=ordinal;contextId=work.context.contextId;stopId=work.context.key.stopId
                               routePointId=work.points.[pointIndex].routePointId
                               attachment=evidence.attachments.[variantIndex].[pointIndex]} }
        let mutable corridorCount=0L
        let mutable attachmentCount=0L
        for struct(_,_,evidence) in joinedRows() do
            corridorCount<-corridorCount+int64 evidence.variants.Length
            attachmentCount<-
                attachmentCount+
                (evidence.attachments |> Array.sumBy(fun values -> int64 values.Length))
        let contextStore=
            new ProjectedRowStore<PostEvidenceContext>(contextCount,contextRows)
            :> IReplayableRowStore<PostEvidenceContext>
        let corridorStore=
            new ProjectedRowStore<PostEvidenceCorridorVariant>(corridorCount,corridorRows)
            :> IReplayableRowStore<PostEvidenceCorridorVariant>
        let attachmentStore=
            new ProjectedRowStore<PostEvidenceRoutePointAttachment>(attachmentCount,attachmentRows)
            :> IReplayableRowStore<PostEvidenceRoutePointAttachment>
        let transientPeak=
            routePointPreparationPeak+workPreparationPeak+routed.PeakSpillBytes
        options.progress "capture-routing-contexts" routingEvidenceCount
            (Some routingEvidenceCount)
            (Some $"logical_contexts={contextCount}; current_spill_bytes={observationStore.CurrentSpillBytes+routePointStore.CurrentSpillBytes+orderedWork.CurrentSpillBytes+routed.CurrentSpillBytes}; peak_spill_bytes={transientPeak+observationStore.PeakSpillBytes+routePointStore.PeakSpillBytes}")
        let captured=new CapturedPostEvidence(
            observationStore,routePointStore,contextStore,corridorStore,attachmentStore,
            [|orderedWork :> IDisposable;routed :> IDisposable|],
            (fun () -> orderedWork.CurrentSpillBytes+routed.CurrentSpillBytes),
            transientPeak)
        let validated=validateCapturedEvidence captured
        owned.Clear()
        routedStore<-None
        orderedWorkOwnership.Transfer()
        preparationOwnership.Transfer()
        validated
    with _ ->
        routedStore |> Option.iter _.Dispose()
        for value in owned do value.Dispose()
        reraise()

let captureToStore options graph batch =
    captureToStoreWithCancellation CancellationToken.None options graph batch
