// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.GeoData.Osm

#nowarn "9"

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.IO.MemoryMappedFiles
open System.Runtime
open System.Runtime.CompilerServices
open System.Text
open System.Threading
open System.Threading.Tasks
open OsmSharp
open OsmSharp.Geo
open OsmSharp.Streams
open NetTopologySuite.Features
open NetTopologySuite.Geometries
open Serilog

open JrUtil.GeoData.CzRegions
open JrUtil.GeoData.Common
open JrUtil.GeoData.StopMatcher
open JrUtil.JdfModel
open JrUtil.JdfFixups
open JrUtil.Utils
open Microsoft.FSharp.NativeInterop

type CzOtherStop = {
    id: int64
    name: string option
    officialName: string option
    point: Point
    rawTags: string
    explicitModes: string
    deniedModes: string
    lifecycle: string
}

let featTagOpt name (feat: IFeature) =
    feat.Attributes.GetOptionalValue(name)
    |> nullOpt
    |> Option.map unbox<string>

let getCzRailStops pbfPath = cacheVoidFunc "cz-osm-rail-stops" <| fun () ->
    use stream = File.OpenRead(pbfPath)
    (new PBFOsmStreamSource(stream)
     |> Seq.filter (fun node ->
         node.Type = OsmGeoType.Node
         && (node.Tags.Contains("railway", "halt")
             || node.Tags.Contains("railway", "station"))
         && not (node.Tags.ContainsKey("subway"))))
     .ToFeatureSource()
    |> Seq.map (fun feat -> {|
        id = feat.Attributes.["id"] :?> int64
        sr70 =
            featTagOpt "ref:sr70" feat
            |> Option.orElse (featTagOpt "railway:ref" feat)
            |> Option.map normaliseSr70
        name =
            featTagOpt "name" feat
            |> Option.orElse (featTagOpt "name:cs" feat)
        point = wgs84Factory.CreateGeometry(feat.Geometry) :?> Point
    |})
    |> Seq.toArray

let czOtherStopNameRegion =
    // Used for synonym matching
    let matcher = new StopMatcher<_>([||])
    fun (stop: CzOtherStop) etrs89ExPt ->
        // Stops within cities often omit the city's name on the pole, so we
        // have to add it back in. We assume official_name to be the full name.
        match stop.officialName,
              stop.name,
              czechTownByPoint () etrs89ExPt with
        | None, None, _ -> "", None
        | Some n, _, None -> n, None
        | Some n, _, Some (_, r, _) -> n, Some r
        | None, Some n, None -> n, None
        | None, Some n, Some (tn, r, _) ->
            if matcher.nameSimilarity(
                stopNameToTokens n, stopNameToTokens tn) = 1f
            then n, Some r
            else tn + "," + n, Some r

let getCzOtherStops pbfPath = cacheVoidFunc "cz-osm-other-stops" <| fun () ->
    use stream = File.OpenRead(pbfPath)
    (new PBFOsmStreamSource(stream)
     |> Seq.filter (fun node ->
         node.Type = OsmGeoType.Node
         && not (node.Tags.ContainsKey("train"))
         && (not (node.Tags.ContainsKey("railway"))
             || node.Tags.ContainsKey("tram"))
         && (node.Tags.Contains("highway", "bus_stop")
             || node.Tags.Contains("public_transport", "platform")
             || node.Tags.Contains("public_transport", "pole")
             || node.Tags.Contains("public_transport", "station")
             || node.Tags.Contains("railway", "tram_stop")
             || node.Tags.Contains("amenity", "bus_station"))))
     .ToFeatureSource()
    |> Seq.map (fun feat ->
        let tag name = featTagOpt name feat
        let valueIs (expected: string) = function
            | Some (value: string) -> String.Equals(value, expected, StringComparison.OrdinalIgnoreCase)
            | None -> false
        let affirmative = function
            | Some (value: string) ->
                match value.Trim().ToLowerInvariant() with
                | "yes" | "designated" | "official" -> true
                | _ -> false
            | None -> false
        let negative = function
            | Some (value: string) -> String.Equals(value.Trim(), "no", StringComparison.OrdinalIgnoreCase)
            | None -> false
        let bus = tag "bus"
        let psv = tag "psv"
        let tram = tag "tram"
        let highway = tag "highway"
        let railway = tag "railway"
        let amenity = tag "amenity"
        let explicitModes =
            [ if valueIs "bus_stop" highway || valueIs "bus_station" amenity
                 || affirmative bus || affirmative psv then
                  "road"
              if valueIs "tram_stop" railway || affirmative tram then
                  "tram" ]
            |> String.concat ";"
        let deniedModes =
            [ if negative bus || negative psv then
                  "road"
              if negative tram then
                  "tram" ]
            |> String.concat ";"
        let lifecycle =
            [ "abandoned"; "disused"; "construction"; "proposed"; "temporary" ]
            |> List.tryFind (fun key ->
                feat.Attributes.Exists(key)
                || (tag "lifecycle" |> valueIs key))
            |> Option.defaultValue "active"
        let auditKeys = [|
            "highway"; "public_transport"; "railway"; "amenity"
            "bus"; "psv"; "tram"; "trolleybus"
            "access"; "vehicle"; "motor_vehicle"
            "abandoned"; "disused"; "construction"; "proposed"; "temporary"
        |]
        let rawTags =
            auditKeys
            |> Array.choose (fun key -> tag key |> Option.map (fun value -> $"{key}={value}"))
            |> String.concat ";"
        {
        id = feat.Attributes.["id"] :?> int64
        name =
            featTagOpt "name" feat
            |> Option.orElse (featTagOpt "name:cs" feat)
        officialName = featTagOpt "official_name" feat
        point = wgs84Factory.CreateGeometry(feat.Geometry) :?> Point
        rawTags = rawTags
        explicitModes = explicitModes
        deniedModes = deniedModes
        lifecycle = lifecycle
    })
    |> Seq.toArray

let czOtherStopsForJdfMatch (stops: CzOtherStop seq) =
    stops
    |> Seq.map (fun s ->
        let etrs89ExPt = pointWgs84ToEtrs89Ex s.point
        let name, region = czOtherStopNameRegion s etrs89ExPt
        {
            name = name
            data = {
                country = if region.IsSome then Some "CZ" else None
                regionId = region
                point = etrs89ExPt
                source = Some $"osm:node:{s.id}"
                candidateObservation = Some {
                    observationId = $"osm:node:{s.id}"
                    sourceKind = "osm"
                    sourceObjectId = Some $"osm:node:{s.id}"
                    observedAt = None
                    rawTags = s.rawTags
                    explicitModes = s.explicitModes
                    deniedModes = s.deniedModes
                    lifecycle = s.lifecycle
                    supportWeight = 1M
                }
            }
        })
    |> Seq.toArray

let private roadHighways =
    set [ "motorway"; "trunk"; "primary"; "secondary"; "tertiary"
          "unclassified"; "residential"; "living_street"; "service"
          "road"; "busway" ]

let private wayTagOpt name (way: Way) =
    let mutable value = null
    if not (isNull way.Tags) && way.Tags.TryGetValue(name, &value)
    then Option.ofObj value
    else None

// Conversion-local directed topology. Osmium is still the sole preprocessing
// engine; this reader only packs the already demand-clipped, type-ordered PBF.
type RoutingMode = RoadBus | Trolleybus | TramRouting
type RoutedDistance = Routed of float | RoutingUnavailable of string

[<Struct; NoComparison; NoEquality>]
type DirectedSnapState = {
    edgeId: int
    fraction: float
    projectedX: float
    projectedY: float
    perpendicularDistance: float
}

[<Struct>]
type DirectedSnapDiagnostic = {
    snapEdgeId: int
    wayId: int64
    snapFraction: float
    projectedLon: float
    projectedLat: float
    snapDistance: float
    localThreadId: string
}

type RoutedCorridorEvidence = {
    availability: string
    unavailableReason: string option
    baselineCost: float option
    corridorId: string option
    ingressThreadId: string option
    egressThreadId: string option
    corridorEdges: int array
    alternativeCorridorCount: int
    alternativeCorridorIds: string array
    alternativeCostGap: float option
}

type CandidateRouteEvidence = {
    routed: RoutedDistance
    snap: DirectedSnapState option
    corridorDistance: float option
    signedLateralOffset: float option
    corridorHeading: float option
    attachmentHeading: float option
    localFaceId: string option
    tiedCorridorsAgree: bool
    alignment: float
    side: float
    proximity: float
    rejectionReason: string option
}

type RoutedCandidateBatch = {
    corridor: RoutedCorridorEvidence
    candidates: CandidateRouteEvidence array
}

/// A policy-neutral directed corridor captured by the router.  Costs and
/// diagnostics are routing facts; no publication threshold is applied here.
type RoutedCorridorVariant = {
    variantRank: int
    corridorId: string option
    absoluteCostMetres: float option
    relativeCostMetres: float option
    relativeCostFraction: float option
    pathLengthMetres: float option
    directedEdgeCount: int
    repeatedDirectedEdgeCount: int
    serviceEdgeCount: int
    restrictedAccessEdgeCount: int
    accessPenaltyMetres: float option
    ingressThreadId: string option
    egressThreadId: string option
    routingAvailability: string
    invariantFailureReason: string option
}

/// Complete attachment evidence for one route point against one captured
/// corridor variant.  Failure rows are retained so absence is never
/// misinterpreted as weak positive evidence during replay.
type RoutePointVariantAttachment = {
    variantRank: int
    corridorId: string option
    routedExcessMetres: float option
    snapEdgeId: int option
    snapWayId: int64 option
    snapFraction: float option
    snapProjectedLongitude: float option
    snapProjectedLatitude: float option
    snapDistanceMetres: float option
    corridorFaceId: string option
    corridorDistanceMetres: float option
    signedLateralOffsetMetres: float option
    corridorHeadingDegrees: float option
    attachmentHeadingDegrees: float option
    headingDifferenceDegrees: float option
    proximityDistanceMetres: float option
    routingAvailability: string
    invariantFailureReason: string option
}

type RoutedContextEvidence = {
    variants: RoutedCorridorVariant array
    attachments: RoutePointVariantAttachment array array
}

[<Literal>]
let PostInferenceEvidenceHorizonMetres = 1000.0

type private RoutedPath = {
    cost: float
    origin: DirectedSnapState
    target: DirectedSnapState
    edges: int array
}

type private RoutingSearchWorkspace = {
    targets: Dictionary<int, ResizeArray<DirectedSnapState>>
    routeDistances: Dictionary<int,float>
    origins: Dictionary<int,DirectedSnapState>
    predecessors: Dictionary<int,int>
    depths: Dictionary<int,int>
    routeQueue: PriorityQueue<struct(int*int),struct(float*int)>
    forwardDistances: Dictionary<int,float>
    forwardParents: Dictionary<int,int>
    forwardQueue: PriorityQueue<int,struct(float*int)>
    reverseDistances: Dictionary<int,float>
    reverseSuccessors: Dictionary<int,int>
    reverseQueue: PriorityQueue<int,struct(float*int)>
}

let private newRoutingSearchWorkspace () = {
    targets=Dictionary<int,ResizeArray<DirectedSnapState>>()
    routeDistances=Dictionary<int,float>(); origins=Dictionary<int,DirectedSnapState>()
    predecessors=Dictionary<int,int>(); depths=Dictionary<int,int>()
    routeQueue=PriorityQueue<struct(int*int),struct(float*int)>()
    forwardDistances=Dictionary<int,float>(); forwardParents=Dictionary<int,int>()
    forwardQueue=PriorityQueue<int,struct(float*int)>()
    reverseDistances=Dictionary<int,float>(); reverseSuccessors=Dictionary<int,int>()
    reverseQueue=PriorityQueue<int,struct(float*int)>()
}

[<Struct; NoComparison; NoEquality>]
type private RoutingNodeBuild = { id: int64; x: float32; y: float32; barrierBlocked: byte }
[<Struct; NoComparison; NoEquality>]
type private RoutingEdgeBuild = {
    fromNode: int; toNode: int; length: float32; wayId: int64; flags: uint16
}
[<Struct; NoComparison; NoEquality>]
type private SnapBuild = { key: int64; edge: int }
[<Struct; NoComparison; NoEquality>]
type private RoutingRestriction = { fromWay: int64; toWay: int64; viaNode: int; only: bool }

let private routingRoad, routingTram, routingSevere = 1us, 2us, 4us
let private routingService = 8us
let private routingNodeStride, routingEdgeStride, routingSnapStride = 24L, 24L, 16L
let private routingCellSize = 250.0
let private normalizedTag value =
    value |> Option.map (fun (text: string) -> text.Trim().ToLowerInvariant())
let private affirmativeAccess value =
    match normalizedTag value with
    | Some ("yes" | "designated" | "official" | "permissive") -> true
    | _ -> false
let private hardNegativeAccess value =
    match normalizedTag value with Some ("no" | "private") -> true | _ -> false
let private routingNodeTagOpt name (node: Node) =
    let mutable value = null
    if not (isNull node.Tags) && node.Tags.TryGetValue(name, &value) then Option.ofObj value else None
let private barrierBlocked (node: Node) =
    let barrier = routingNodeTagOpt "barrier" node |> normalizedTag
    let transitPermission =
        affirmativeAccess (routingNodeTagOpt "bus" node)
        || affirmativeAccess (routingNodeTagOpt "psv" node)
    barrier.IsSome && not transitPermission
let private roadRoutingFlags (way: Way) =
    let highway = wayTagOpt "highway" way |> normalizedTag
    let bus, psv = wayTagOpt "bus" way, wayTagOpt "psv" way
    let specificPermission = affirmativeAccess bus || affirmativeAccess psv
    let generalBlock =
        [ wayTagOpt "access" way; wayTagOpt "vehicle" way; wayTagOpt "motor_vehicle" way ]
        |> List.exists hardNegativeAccess
    let closed =
        highway = Some "construction"
        || [ "construction"; "abandoned"; "disused"; "proposed" ]
           |> List.exists (fun key -> wayTagOpt key way |> Option.isSome)
    if not (highway |> Option.exists roadHighways.Contains)
       || closed || (generalBlock && not specificPermission) then 0us
    else routingRoad
         ||| (if hardNegativeAccess bus || hardNegativeAccess psv then routingSevere else 0us)
         ||| (if highway = Some "service" then routingService else 0us)
let private tramRoutingFlags (way: Way) =
    if wayTagOpt "railway" way |> normalizedTag <> Some "tram" then 0us
    elif [ "construction"; "abandoned"; "disused"; "proposed" ]
         |> List.exists (fun key -> wayTagOpt key way |> Option.isSome) then 0us
    else routingTram
let private routingDirections (way: Way) =
    match wayTagOpt "oneway" way |> normalizedTag with
    | Some ("yes" | "true" | "1") -> true, false
    | Some ("-1" | "reverse") -> false, true
    | _ when wayTagOpt "junction" way |> normalizedTag = Some "roundabout" -> true, false
    | _ -> true, true
let private modeFlag = function RoadBus | Trolleybus -> routingRoad | TramRouting -> routingTram
let private routingGridKey x y = (int64 x <<< 32) ||| int64 (uint32 y)
let private routingGridCoordinate value = int (Math.Floor(value / routingCellSize))

let inline private readMapped<'value when 'value : unmanaged> (pointer: nativeint) (offset: int64) =
    let address = NativePtr.ofNativeInt<byte>(pointer + nativeint offset)
    Unsafe.ReadUnaligned<'value>(NativePtr.toVoidPtr address)

type PackedRoutingGraph private
        (tempDirectory: string, nodeCount: int, edgeCount: int, snapCount: int,
         nodeMap: MemoryMappedFile, nodes: MemoryMappedViewAccessor,
         offsetMap: MemoryMappedFile, offsets: MemoryMappedViewAccessor,
         edgeMap: MemoryMappedFile, edges: MemoryMappedViewAccessor,
         reverseOffsetMap: MemoryMappedFile, reverseOffsets: MemoryMappedViewAccessor,
         reverseEdgeMap: MemoryMappedFile, reverseEdges: MemoryMappedViewAccessor,
         snapMap: MemoryMappedFile, snaps: MemoryMappedViewAccessor,
         restrictions: RoutingRestriction array) =
    let cache = ConcurrentDictionary<struct (byte * int * int * int * int * bool), RoutedDistance>()
    let cacheOrder = ConcurrentQueue<struct (byte * int * int * int * int * bool)>()
    let mutable cacheHits, cacheMisses, searches = 0L, 0L, 0L
    let mutable restrictionLookups, restrictionRulesExamined = 0L, 0L
    let mutable maximumWorkers = 1
    let mutable disposed = false
    let searchWorkspaces = new ThreadLocal<RoutingSearchWorkspace>(newRoutingSearchWorkspace,true)
    let deleteTemporaryDirectory () =
        let mutable attempt = 0
        while Directory.Exists(tempDirectory) && attempt < 6 do
            try Directory.Delete(tempDirectory,true)
            with _ when attempt < 5 -> Thread.Sleep(25)
            attempt <- attempt+1
    let preparedSnaps =
        Dictionary<struct (byte * int64 * int64), DirectedSnapState array>()
    let coordinateKey mode px py =
        let modeKey = match mode with RoadBus -> 0uy | Trolleybus -> 1uy | TramRouting -> 2uy
        struct (modeKey,
                int64 (Math.Round(px * 10.0, MidpointRounding.AwayFromZero)),
                int64 (Math.Round(py * 10.0, MidpointRounding.AwayFromZero)))
    let sortedRestrictions =
        restrictions
        |> Array.sortBy (fun restriction ->
            restriction.viaNode, restriction.fromWay, restriction.only, restriction.toWay)
    let restrictedViaNodes = HashSet<int>(sortedRestrictions |> Seq.map (fun value -> value.viaNode))
    let acquirePointer (accessor: MemoryMappedViewAccessor) =
        let mutable pointer = NativePtr.nullPtr<byte>
        accessor.SafeMemoryMappedViewHandle.AcquirePointer(&pointer)
        NativePtr.toNativeInt pointer + nativeint accessor.PointerOffset
    let nodePointer = acquirePointer nodes
    let offsetPointer = acquirePointer offsets
    let edgePointer = acquirePointer edges
    let reverseOffsetPointer = acquirePointer reverseOffsets
    let reverseEdgePointer = acquirePointer reverseEdges
    let snapPointer = acquirePointer snaps
    let nodeId index = readMapped<int64> nodePointer (int64 index * routingNodeStride)
    let nodeX index = float (readMapped<float32> nodePointer (int64 index * routingNodeStride + 8L))
    let nodeY index = float (readMapped<float32> nodePointer (int64 index * routingNodeStride + 12L))
    let edgeFrom index = readMapped<int> edgePointer (int64 index * routingEdgeStride)
    let edgeTo index = readMapped<int> edgePointer (int64 index * routingEdgeStride + 4L)
    let edgeLength index = float (readMapped<float32> edgePointer (int64 index * routingEdgeStride + 8L))
    let edgeFlags index = readMapped<uint16> edgePointer (int64 index * routingEdgeStride + 20L)
    let edgeWay index = readMapped<int64> edgePointer (int64 index * routingEdgeStride + 12L)
    let outgoingOffset node = readMapped<int64> offsetPointer (int64 node*8L)
    let incomingOffset node = readMapped<int64> reverseOffsetPointer (int64 node*8L)
    let incomingEdge index = readMapped<int> reverseEdgePointer (int64 index*4L)
    let snapKey index = readMapped<int64> snapPointer (int64 index*routingSnapStride)
    let snapEdge index = readMapped<int> snapPointer (int64 index*routingSnapStride+8L)
    let transitionAllowed incoming outgoing viaNode =
        if not (restrictedViaNodes.Contains(viaNode)) then true else
        let incomingWay, outgoingWay = edgeWay incoming, edgeWay outgoing
        Interlocked.Increment(&restrictionLookups) |> ignore
        let compareKey (restriction: RoutingRestriction) =
            let viaComparison = compare restriction.viaNode viaNode
            if viaComparison <> 0 then viaComparison
            else compare restriction.fromWay incomingWay
        let mutable low, high = 0, sortedRestrictions.Length - 1
        while low <= high do
            let middle = low + (high-low)/2
            if compareKey sortedRestrictions.[middle] >= 0 then high <- middle - 1
            else low <- middle + 1
        let mutable index, allowed = low, true
        while allowed && index < sortedRestrictions.Length
              && compareKey sortedRestrictions.[index] = 0 do
            let restriction = sortedRestrictions.[index]
            Interlocked.Increment(&restrictionRulesExamined) |> ignore
            allowed <-
                if restriction.only then restriction.toWay = outgoingWay
                else restriction.toWay <> outgoingWay
            index <- index + 1
        allowed
    let edgePenalty index = if edgeFlags index &&& routingSevere <> 0us then 8.0 else 1.0
    let edgeCost index = edgePenalty index * edgeLength index
    let pointSegmentSnap px py edgeIndex =
        let left, right = edgeFrom edgeIndex, edgeTo edgeIndex
        let x0, y0, x1, y1 = nodeX left, nodeY left, nodeX right, nodeY right
        let dx, dy = x1 - x0, y1 - y0
        let lengthSquared = dx * dx + dy * dy
        let fraction = if lengthSquared = 0.0 then 0.0 else max 0.0 (min 1.0 (((px-x0)*dx+(py-y0)*dy)/lengthSquared))
        let x, y = x0 + fraction * dx, y0 + fraction * dy
        { edgeId = edgeIndex
          fraction = fraction
          projectedX = x
          projectedY = y
          perpendicularDistance = Math.Sqrt((px-x) ** 2.0 + (py-y) ** 2.0) }
    let snapRange key =
        let mutable low, high, first = 0, snapCount - 1, snapCount
        while low <= high do
            let middle = low + (high-low)/2
            let value = snapKey middle
            if value >= key then
                if value = key then first <- middle
                high <- middle - 1
            else low <- middle + 1
        if first = snapCount then ValueNone else
        let mutable finish = first + 1
        while finish < snapCount && snapKey finish = key do finish <- finish + 1
        ValueSome(struct(first, finish))
    let snap mode px py =
        match preparedSnaps.TryGetValue(coordinateKey mode px py) with
        | true, values -> values
        | _ ->
            let required = modeFlag mode
            let values, seen = ResizeArray<DirectedSnapState>(), HashSet<int>()
            let cx, cy = routingGridCoordinate px, routingGridCoordinate py
            for x = cx-1 to cx+1 do
                for y = cy-1 to cy+1 do
                    match snapRange (routingGridKey x y) with
                    | ValueSome(struct(first, finish)) ->
                        for index = first to finish-1 do
                            let edge = snapEdge index
                            if seen.Add(edge) && edgeFlags edge &&& required <> 0us then
                                let state = pointSegmentSnap px py edge
                                if state.perpendicularDistance <= 75.0 then values.Add(state)
                    | ValueNone -> ()
            let ordered =
                values
                |> Seq.sortBy (fun state -> state.perpendicularDistance,state.edgeId,state.fraction)
                |> Seq.truncate 8 |> Seq.toArray
            if ordered.Length=0 then ordered else
            let maximumPlausibleDistance=ordered.[0].perpendicularDistance+15.0
            ordered |> Array.filter (fun state -> state.perpendicularDistance<=maximumPlausibleDistance)
    let routeDetailed mode (startSnaps: DirectedSnapState array)
                           (finishSnaps: DirectedSnapState array) requireTransition =
        Interlocked.Increment(&searches) |> ignore
        if startSnaps.Length = 0 || finishSnaps.Length = 0 then
            RoutingUnavailable "snap-unavailable", None
        else
        let required = modeFlag mode
        let workspace=searchWorkspaces.Value
        let targets=workspace.targets
        let distances=workspace.routeDistances
        let origins=workspace.origins
        let predecessors=workspace.predecessors
        let depths=workspace.depths
        let queue=workspace.routeQueue
        targets.Clear(); distances.Clear(); origins.Clear(); predecessors.Clear(); depths.Clear(); queue.Clear()
        for state in finishSnaps do
            match targets.TryGetValue(state.edgeId) with
            | true, values -> values.Add(state)
            | _ -> targets.[state.edgeId] <- ResizeArray([state])
        let targetNodes =
            finishSnaps
            |> Array.collect (fun state -> [|edgeFrom state.edgeId; edgeTo state.edgeId|])
            |> Array.distinct
        // Every target snap is local to one stop.  Distance to the enclosing
        // target circle is an admissible O(1) lower bound; checking all (up to
        // sixteen) target endpoints at every expansion dominated national A*.
        let targetCentreX,targetCentreY =
            if targetNodes.Length=0 then 0.0,0.0 else
            targetNodes |> Array.averageBy nodeX,
            targetNodes |> Array.averageBy nodeY
        let targetRadius =
            targetNodes
            |> Array.map (fun target ->
                let dx,dy=nodeX target-targetCentreX,nodeY target-targetCentreY
                Math.Sqrt(dx*dx+dy*dy))
            |> function [||] -> 0.0 | values -> Array.max values
        let heuristic node =
            let dx,dy=nodeX node-targetCentreX,nodeY node-targetCentreY
            max 0.0 (Math.Sqrt(dx*dx+dy*dy)-targetRadius)
        for state in startSnaps do
            let edge = state.edgeId
            let initial = (1.0-state.fraction) * edgeCost edge
            match distances.TryGetValue(edge) with
            | true, previous when previous <= initial -> ()
            | _ ->
                distances.[edge] <- initial
                origins.[edge] <- state
                depths.[edge] <- 1
                queue.Enqueue(struct(edge,1), struct(initial + heuristic (edgeTo edge),edge))
        let mutable result, resultEdge, explored = Double.PositiveInfinity, -1, 0
        let mutable resultOrigin = Unchecked.defaultof<DirectedSnapState>
        let mutable resultTarget = Unchecked.defaultof<DirectedSnapState>
        while queue.Count > 0 && explored < 100_000 do
            let mutable queuedElement = Unchecked.defaultof<struct(int*int)>
            let mutable queuedPriority = Unchecked.defaultof<struct(float*int)>
            queue.TryDequeue(&queuedElement, &queuedPriority) |> ignore
            let struct(edge,depth) = queuedElement
            let struct(queuedEstimate,_) = queuedPriority
            let distance = distances.[edge]
            // PriorityQueue keeps superseded entries. Expanding them can multiply the
            // work on dense road graphs without changing the shortest path.
            let currentEstimate = distance + heuristic (edgeTo edge)
            if queuedEstimate <= currentEstimate + 0.0001
               && distance < result && distance <= 30_000.0 then
                explored <- explored + 1
                match targets.TryGetValue(edge) with
                | true, targetStates when not requireTransition || depth > 1 ->
                    for target in targetStates do
                        // The state distance is measured at edgeTo. Remove the
                        // untravelled suffix after the fractional target.
                        let candidate = distance - (1.0-target.fraction) * edgeCost edge
                        if candidate >= -0.0001 then
                            let candidate = max 0.0 candidate
                            let origin = origins.[edge]
                            let stable = struct(origin.edgeId,target.edgeId,origin.fraction,target.fraction)
                            let previousStable =
                                struct(resultOrigin.edgeId,resultTarget.edgeId,
                                       resultOrigin.fraction,resultTarget.fraction)
                            if candidate < result
                               || (candidate = result
                                   && (resultEdge < 0 || stable < previousStable)) then
                                result <- candidate
                                resultOrigin <- origin
                                resultTarget <- target
                                resultEdge <- edge
                | _ -> ()
                let node = edgeTo edge
                let first = int (outgoingOffset node)
                let finish = int (outgoingOffset (node+1))
                for outgoing = first to finish-1 do
                    let flags = edgeFlags outgoing
                    if flags &&& required <> 0us && transitionAllowed edge outgoing node then
                        let candidate = distance + edgeCost outgoing
                        match distances.TryGetValue(outgoing) with
                        | true, old when old <= candidate -> ()
                        | _ ->
                            distances.[outgoing] <- candidate
                            origins.[outgoing] <- origins.[edge]
                            predecessors.[outgoing] <- edge
                            depths.[outgoing] <- depth+1
                            queue.Enqueue(struct(outgoing,depth+1),
                                          struct(candidate + heuristic (edgeTo outgoing),outgoing))
        if Double.IsFinite(result) then
            let reversed = ResizeArray<int>()
            let mutable current = resultEdge
            reversed.Add(current)
            while current <> resultOrigin.edgeId && predecessors.ContainsKey(current) do
                current <- predecessors.[current]
                reversed.Add(current)
            let path = reversed |> Seq.rev |> Seq.toArray
            Routed result,
            Some { cost=result; origin=resultOrigin; target=resultTarget; edges=path }
        elif explored >= 100_000 then RoutingUnavailable "search-limit", None
        else RoutingUnavailable "disconnected", None
    let route mode startSnaps finishSnaps requireTransition =
        routeDetailed mode startSnaps finishSnaps requireTransition |> fst
    let exploreForward mode (startSnap: DirectedSnapState) maximumCost =
        Interlocked.Increment(&searches) |> ignore
        let required = modeFlag mode
        let workspace=searchWorkspaces.Value
        let distances=workspace.forwardDistances
        let parents=workspace.forwardParents
        let queue=workspace.forwardQueue
        distances.Clear(); parents.Clear(); queue.Clear()
        let startEdge = startSnap.edgeId
        let initial = (1.0-startSnap.fraction)*edgeCost startEdge
        distances.[startEdge] <- initial
        queue.Enqueue(startEdge,struct(initial,startEdge))
        let mutable explored = 0
        while queue.Count > 0 && explored < 100_000 do
            let mutable edge = 0
            let mutable priority = Unchecked.defaultof<struct(float*int)>
            queue.TryDequeue(&edge,&priority) |> ignore
            let struct (queuedDistance,_) = priority
            let distance = distances.[edge]
            if queuedDistance <= distance+0.0001 && distance <= maximumCost then
                explored <- explored+1
                let node = edgeTo edge
                let first = int (outgoingOffset node)
                let finish = int (outgoingOffset (node+1))
                for outgoing=first to finish-1 do
                    let flags=edgeFlags outgoing
                    if flags &&& required <> 0us && transitionAllowed edge outgoing node then
                        let candidate=distance+edgeCost outgoing
                        match distances.TryGetValue(outgoing) with
                        | true,old when old <= candidate -> ()
                        | _ ->
                            distances.[outgoing] <- candidate
                            parents.[outgoing] <- edge
                            queue.Enqueue(outgoing,struct(candidate,outgoing))
        distances,parents,explored >= 100_000
    let exploreReverse mode (finishSnap: DirectedSnapState) maximumCost =
        Interlocked.Increment(&searches) |> ignore
        let required = modeFlag mode
        // Value is the cost after completing this edge through the fixed
        // outgoing anchor. The edge's own length is added by the caller.
        let workspace=searchWorkspaces.Value
        let distances=workspace.reverseDistances
        let successors=workspace.reverseSuccessors
        let queue=workspace.reverseQueue
        distances.Clear(); successors.Clear(); queue.Clear()
        let finishEdge = finishSnap.edgeId
        let initial = finishSnap.fraction*edgeCost finishEdge
        distances.[finishEdge] <- initial
        queue.Enqueue(finishEdge,struct(initial,finishEdge))
        let mutable explored = 0
        while queue.Count > 0 && explored < 100_000 do
            let mutable edge = 0
            let mutable priority = Unchecked.defaultof<struct(float*int)>
            queue.TryDequeue(&edge,&priority) |> ignore
            let struct (queuedDistance,_) = priority
            let distance = distances.[edge]
            if queuedDistance <= distance+0.0001 && distance <= maximumCost then
                explored <- explored+1
                let node = edgeFrom edge
                let first = int (incomingOffset node)
                let finish = int (incomingOffset (node+1))
                for index=first to finish-1 do
                    let incoming = incomingEdge index
                    if edgeFlags incoming &&& required <> 0us
                       && transitionAllowed incoming edge node then
                        // Distance is measured at edgeFrom. Moving to the
                        // predecessor therefore adds the predecessor cost.
                        let candidate=distance+edgeCost incoming
                        match distances.TryGetValue(incoming) with
                        | true,old when old <= candidate -> ()
                        | _ ->
                            distances.[incoming] <- candidate
                            successors.[incoming] <- edge
                            queue.Enqueue(incoming,struct(candidate,incoming))
        distances,successors,explored >= 100_000
    let parallelMap (mapping: 'a -> 'b) (values: 'a array) =
        if maximumWorkers <= 1 || values.Length <= 1 then Array.map mapping values
        else
            let results = Array.zeroCreate<'b> values.Length
            let options = ParallelOptions(MaxDegreeOfParallelism = maximumWorkers)
            Parallel.For(0, values.Length, options, fun index ->
                results.[index] <- mapping values.[index])
            |> ignore
            results
    let angleDifference left right =
        let difference = abs(left-right) % 360.0
        min difference (360.0-difference)
    let headingForEdge edge =
        let left,right=edgeFrom edge,edgeTo edge
        (Math.Atan2(nodeX right-nodeX left,nodeY right-nodeY left)*180.0/Math.PI+360.0)%360.0
    let corridorIdentity (path: int array) =
        let mutable hash=14695981039346656037UL
        for edge in path do hash <- (hash ^^^ uint64(uint32 edge))*1099511628211UL
        hash.ToString("x16",Globalization.CultureInfo.InvariantCulture)
    let localThreadCache=ConcurrentDictionary<int,struct(string*bool)>()
    let threadEndCache=ConcurrentDictionary<struct(uint16*int*int),struct(int64*bool)>()
    let localThreadIdentity edge =
        localThreadCache.GetOrAdd(edge,fun seedEdge ->
            let modeMask=edgeFlags seedEdge &&& (routingRoad ||| routingTram)
            let roadClass=if edgeFlags seedEdge &&& routingService<>0us then routingService else 0us
            let neighbours node previous =
                let values=ResizeArray<int>(4)
                let add value =
                    if value<>previous && not(values.Contains(value)) then values.Add(value)
                let mutable offset=outgoingOffset node
                let finish=outgoingOffset(node+1)
                while offset<finish do
                    let candidate=int offset
                    if edgeFlags candidate &&& modeMask<>0us
                       && (modeMask=routingTram
                           || edgeFlags candidate &&& routingService=roadClass) then add(edgeTo candidate)
                    offset<-offset+1L
                let mutable reverse=incomingOffset node
                let reverseFinish=incomingOffset(node+1)
                while reverse<reverseFinish do
                    let candidate=incomingEdge(int reverse)
                    if edgeFlags candidate &&& modeMask<>0us
                       && (modeMask=routingTram
                           || edgeFlags candidate &&& routingService=roadClass) then add(edgeFrom candidate)
                    reverse<-reverse+1L
                values
            let walk previous start =
                let mutable prior=previous
                let mutable current=start
                let mutable count=0
                let mutable loop=false
                let mutable minimum=nodeId current
                let mutable complete=false
                let mutable cachedEnd=None
                let visited=ResizeArray<struct(uint16*int*int)>()
                while not complete && count<512 do
                    let cacheKey=struct(modeMask,prior,current)
                    match threadEndCache.TryGetValue(cacheKey) with
                    | true,value ->
                        cachedEnd<-Some value
                        complete<-true
                    | _ ->
                        visited.Add(cacheKey)
                        minimum<-min minimum (nodeId current)
                        let next=neighbours current prior
                        if next.Count<>1 then complete<-true
                        else
                            let value=next.[0]
                            if value=start || value=previous then
                                loop<-true
                                complete<-true
                            else
                                prior<-current
                                current<-value
                                count<-count+1
                let result =
                    match cachedEnd with
                    | Some value -> value
                    | None when loop || count>=512 -> struct(minimum,true)
                    | None -> struct(nodeId current,false)
                for key in visited do threadEndCache.TryAdd(key,result) |> ignore
                result
            let left,right=edgeFrom seedEdge,edgeTo seedEdge
            let struct(leftEnd,leftLoop)=walk right left
            let struct(rightEnd,rightLoop)=walk left right
            let first,last=min leftEnd rightEnd,max leftEnd rightEnd
            let payload = $"{modeMask}|{roadClass}|{first}|{last}|{leftLoop || rightLoop}"
            let bytes=Text.Encoding.UTF8.GetBytes(payload)
            let hash=Security.Cryptography.SHA256.HashData(bytes)
            let identity=Convert.ToHexString(hash.AsSpan(0,16)).ToLowerInvariant()
            let reversed =
                if leftLoop || rightLoop then nodeId left>nodeId right
                else leftEnd>rightEnd
            struct(identity,reversed))
    let localFaceIdentity edge =
        let struct(identity,_)=localThreadIdentity edge
        identity
    let directedThreadIdentity edge =
        let struct(identity,reversed)=localThreadIdentity edge
        let direction=if reversed then "-" else "+"
        $"{identity}:{direction}"
    let candidateGeometry (path: int array) px py attachment =
        if path.Length=0 then None else
        // Bind geometry to the exact directed fractional state which routing
        // selected. A loop or station path can visit the same area several
        // times; choosing the globally nearest segment manufactures the wrong
        // tangent/side and can reverse an otherwise correct routed decision.
        let matchingVisits =
            path
            |> Array.mapi (fun index edge -> index,edge)
            |> Array.filter (fun (_,edge) -> edge=attachment.edgeId)
        // A repeated visit to the same directed edge cannot yet be assigned to
        // one chainage occurrence without carrying the path-state occurrence
        // through the search.  Abstain instead of silently borrowing the first
        // visit's tangent (which is especially dangerous on terminal loops).
        if matchingVisits.Length<>1 then None else
        let localIndex = fst matchingVisits.[0]
        let local = attachment
        let edge=local.edgeId
        let left,right=edgeFrom edge,edgeTo edge
        let pointAtWindow forward =
            let mutable index=localIndex
            let mutable fraction=local.fraction
            let mutable remaining=15.0
            let mutable x,y=local.projectedX,local.projectedY
            let mutable complete=false
            while not complete && index>=0 && index<path.Length do
                let current=path.[index]
                let fromNode,toNode=edgeFrom current,edgeTo current
                let length=max 0.000001(edgeLength current)
                let available=(if forward then 1.0-fraction else fraction)*length
                if remaining<=available then
                    let delta=remaining/length
                    let targetFraction=if forward then fraction+delta else fraction-delta
                    x<-nodeX fromNode+targetFraction*(nodeX toNode-nodeX fromNode)
                    y<-nodeY fromNode+targetFraction*(nodeY toNode-nodeY fromNode)
                    complete<-true
                else
                    remaining<-remaining-available
                    if forward then
                        x<-nodeX toNode; y<-nodeY toNode; index<-index+1; fraction<-0.0
                    else
                        x<-nodeX fromNode; y<-nodeY fromNode; index<-index-1; fraction<-1.0
            x,y
        let beforeX,beforeY=pointAtWindow false
        let afterX,afterY=pointAtWindow true
        let edgeDx,edgeDy=nodeX right-nodeX left,nodeY right-nodeY left
        let windowDx,windowDy=afterX-beforeX,afterY-beforeY
        let dx,dy =
            if windowDx*windowDx+windowDy*windowDy<0.000001 then edgeDx,edgeDy
            else windowDx,windowDy
        let length=max 0.000001 (Math.Sqrt(dx*dx+dy*dy))
        let signed=(dx*(py-local.projectedY)-dy*(px-local.projectedX))/length
        let corridorHeading=(Math.Atan2(dx,dy)*180.0/Math.PI+360.0)%360.0
        let attachmentHeading=headingForEdge attachment.edgeId
        let alignment=max 0.0 (1.0-angleDifference corridorHeading attachmentHeading/90.0)
        let side=if abs signed<=2.0 then 0.5 elif signed<0.0 then 1.0 else 0.0
        let proximity=max 0.0 (1.0-local.perpendicularDistance/50.0)
        let struct(threadIdentity,canonicalReversed)=localThreadIdentity edge
        let edgeSigned=(edgeDx*(py-local.projectedY)-edgeDy*(px-local.projectedX))
                       / max 0.000001(Math.Sqrt(edgeDx*edgeDx+edgeDy*edgeDy))
        let absoluteSigned=if canonicalReversed then -edgeSigned else edgeSigned
        let absoluteSide=if abs absoluteSigned<=2.0 then "C" elif absoluteSigned<0.0 then "R" else "L"
        Some(local.perpendicularDistance,signed,corridorHeading,attachmentHeading,
             $"{threadIdentity}:{absoluteSide}",alignment,side,proximity)
    let candidateAttachmentGeometry px py attachment =
        let edge=attachment.edgeId
        let left,right=edgeFrom edge,edgeTo edge
        let dx,dy=nodeX right-nodeX left,nodeY right-nodeY left
        let length=max 0.000001(Math.Sqrt(dx*dx+dy*dy))
        let signed=(dx*(py-attachment.projectedY)-dy*(px-attachment.projectedX))/length
        let heading=headingForEdge edge
        let side=if abs signed<=2.0 then 0.5 elif signed<0.0 then 1.0 else 0.0
        let proximity=max 0.0(1.0-attachment.perpendicularDistance/50.0)
        let struct(threadIdentity,canonicalReversed)=localThreadIdentity edge
        let absoluteSigned=if canonicalReversed then -signed else signed
        let absoluteSide=if abs absoluteSigned<=2.0 then "C" elif absoluteSigned<0.0 then "R" else "L"
        attachment.perpendicularDistance,signed,heading,heading,
        $"{threadIdentity}:{absoluteSide}",1.0,side,proximity
    let unavailableCandidate reason =
        { routed=RoutingUnavailable reason; snap=None; corridorDistance=None
          signedLateralOffset=None; corridorHeading=None; attachmentHeading=None
          localFaceId=None; tiedCorridorsAgree=false
          alignment=0.0; side=0.0; proximity=0.0; rejectionReason=Some reason }

    let routeContextEvidence mode
                             (previousCoordinates: struct(float*float) array)
                             (candidateCoordinates: struct(float*float) array)
                             (nextCoordinates: struct(float*float) array)
                             turnback =
        let projected lon lat =
            let mutable x,y=lon,lat
            wgs84ToEtrs89Ex.Transform(&x,&y)
            struct(x,y)
        let canonicalAnchorSnaps coordinates =
            coordinates
            |> Array.collect(fun struct(lon,lat) ->
                let struct(x,y)=projected lon lat
                snap mode x y |> Array.truncate 4)
            |> Array.distinctBy(fun state -> state.edgeId,state.fraction)
            |> Array.groupBy(fun state -> directedThreadIdentity state.edgeId)
            |> Array.map(fun (_,values) ->
                values |> Array.minBy(fun state -> state.perpendicularDistance,state.edgeId,state.fraction))
            |> Array.sortBy(fun state -> state.perpendicularDistance,state.edgeId,state.fraction)
            // The input demand is already local.  This ceiling prevents an
            // anomalous dense station from multiplying searches without
            // applying any publication-policy threshold.
            |> Array.truncate 8
        let starts=canonicalAnchorSnaps previousCoordinates
        let finishes=canonicalAnchorSnaps nextCoordinates
        let paths =
            if starts.Length=0 || finishes.Length=0 then [||] else
            [| for start in starts do
                   for finish in finishes do
                       match routeDetailed mode [|start|] [|finish|] turnback with
                       | Routed _,Some path -> yield path
                       | _ -> () |]
            |> Array.distinctBy(fun path -> corridorIdentity path.edges)
            |> Array.sortBy(fun path -> path.cost,corridorIdentity path.edges)
            |> Array.truncate 3
        let failureVariant reason = {
            variantRank=0;corridorId=None;absoluteCostMetres=None
            relativeCostMetres=None;relativeCostFraction=None;pathLengthMetres=None
            directedEdgeCount=0;repeatedDirectedEdgeCount=0;serviceEdgeCount=0
            restrictedAccessEdgeCount=0;accessPenaltyMetres=None
            ingressThreadId=None;egressThreadId=None;routingAvailability="unavailable"
            invariantFailureReason=Some reason }
        let failedAttachment rank corridorId reason = {
            variantRank=rank;corridorId=corridorId;routedExcessMetres=None
            snapEdgeId=None;snapWayId=None;snapFraction=None
            snapProjectedLongitude=None;snapProjectedLatitude=None;snapDistanceMetres=None
            corridorFaceId=None;corridorDistanceMetres=None;signedLateralOffsetMetres=None
            corridorHeadingDegrees=None;attachmentHeadingDegrees=None
            headingDifferenceDegrees=None;proximityDistanceMetres=None
            routingAvailability="unavailable";invariantFailureReason=Some reason }
        if paths.Length=0 then
            { variants=[|failureVariant(if starts.Length=0 || finishes.Length=0 then "snap-unavailable" else "disconnected")|]
              attachments=[|candidateCoordinates |> Array.map(fun _ -> failedAttachment 0 None "baseline-unavailable")|] }
        else
        let baseline=paths.[0].cost
        let variants =
            paths |> Array.mapi(fun rank path ->
                let pathLength=path.edges |> Array.sumBy edgeLength
                let repeated=path.edges.Length-(path.edges |> Array.distinct |> Array.length)
                let service=path.edges |> Array.filter(fun edge -> edgeFlags edge &&& routingService<>0us) |> Array.length
                let restricted=path.edges |> Array.filter(fun edge -> edgeFlags edge &&& routingSevere<>0us) |> Array.length
                let relative=max 0.0(path.cost-baseline)
                { variantRank=rank;corridorId=Some(corridorIdentity path.edges)
                  absoluteCostMetres=Some path.cost;relativeCostMetres=Some relative
                  relativeCostFraction=Some(if baseline<=0.0 then 0.0 else relative/baseline)
                  pathLengthMetres=Some pathLength;directedEdgeCount=path.edges.Length
                  repeatedDirectedEdgeCount=repeated;serviceEdgeCount=service
                  restrictedAccessEdgeCount=restricted
                  accessPenaltyMetres=Some(max 0.0(path.cost-pathLength))
                  ingressThreadId=path.edges |> Array.tryHead |> Option.map directedThreadIdentity
                  egressThreadId=path.edges |> Array.tryLast |> Option.map directedThreadIdentity
                  routingAvailability="available";invariantFailureReason=None })
        let projectedCandidates=candidateCoordinates |> Array.map(fun struct(lon,lat) -> projected lon lat)
        let attachments =
            paths |> Array.mapi(fun rank path ->
                let corridorId=Some(corridorIdentity path.edges)
                let maximumCost=path.cost+PostInferenceEvidenceHorizonMetres
                let forward,parents,forwardLimit=exploreForward mode path.origin maximumCost
                let reverse,successors,reverseLimit=exploreReverse mode path.target maximumCost
                Array.map2(fun struct(candidateX,candidateY) _ ->
                    if forwardLimit || reverseLimit then failedAttachment rank corridorId "search-limit" else
                    let candidates=
                        snap mode candidateX candidateY
                        |> Array.choose(fun state ->
                            match forward.TryGetValue(state.edgeId),reverse.TryGetValue(state.edgeId) with
                            | (true,leftEnd),(true,rightFrom) ->
                                let left=leftEnd-(1.0-state.fraction)*edgeCost state.edgeId
                                let right=rightFrom-state.fraction*edgeCost state.edgeId
                                if left < -0.0001 || right < -0.0001 then None
                                else Some(max 0.0 left+max 0.0 right,state)
                            | _ -> None)
                        |> Array.sortBy(fun (cost,state) -> cost,state.perpendicularDistance,state.edgeId,state.fraction)
                    match candidates with
                    | [||] -> failedAttachment rank corridorId "routed-excess-or-disconnected"
                    | values ->
                        let viaCost,state=values.[0]
                        let excess=max 0.0(viaCost-path.cost)
                        if excess>PostInferenceEvidenceHorizonMetres then
                            failedAttachment rank corridorId "evidence-horizon"
                        else
                        let candidatePath () =
                            let before=ResizeArray<int>()
                            let mutable current=state.edgeId
                            before.Add(current)
                            while parents.ContainsKey(current) do
                                current<-parents.[current]
                                before.Add(current)
                            let after=ResizeArray<int>()
                            current<-state.edgeId
                            while successors.ContainsKey(current) do
                                current<-successors.[current]
                                after.Add(current)
                            Array.append(before |> Seq.rev |> Seq.toArray)(after.ToArray())
                        match candidateGeometry path.edges candidateX candidateY state
                              |> Option.orElseWith(fun () -> candidateGeometry(candidatePath()) candidateX candidateY state) with
                        | None -> failedAttachment rank corridorId "corridor-unavailable"
                        | Some(distance,signed,corridorHeading,attachmentHeading,face,_,_,_) ->
                            let mutable lon,lat=state.projectedX,state.projectedY
                            etrs89ExToWgs84.Transform(&lon,&lat)
                            { variantRank=rank;corridorId=corridorId;routedExcessMetres=Some excess
                              snapEdgeId=Some state.edgeId;snapWayId=Some(edgeWay state.edgeId)
                              snapFraction=Some state.fraction;snapProjectedLongitude=Some lon
                              snapProjectedLatitude=Some lat;snapDistanceMetres=Some state.perpendicularDistance
                              corridorFaceId=Some face;corridorDistanceMetres=Some distance
                              signedLateralOffsetMetres=Some signed
                              corridorHeadingDegrees=Some corridorHeading
                              attachmentHeadingDegrees=Some attachmentHeading
                              headingDifferenceDegrees=Some(angleDifference corridorHeading attachmentHeading)
                              proximityDistanceMetres=Some distance;routingAvailability="available"
                              invariantFailureReason=None }) projectedCandidates candidateCoordinates)
        { variants=variants;attachments=attachments }
    let routeViaManyEvidence mode
                             (previousCoordinates: struct(float*float) array)
                             (candidateCoordinates: struct(float*float) array)
                             (nextCoordinates: struct(float*float) array)
                             turnback =
        let projected lon lat =
            let mutable x,y = lon,lat
            wgs84ToEtrs89Ex.Transform(&x,&y)
            struct (x,y)
        let anchorSnaps coordinates =
            coordinates
            |> Array.collect (fun struct(lon,lat) ->
                let struct(x,y)=projected lon lat
                snap mode x y |> Array.truncate 4)
            |> Array.distinctBy (fun state -> state.edgeId,state.fraction)
            |> Array.sortBy (fun state -> state.perpendicularDistance,state.edgeId,state.fraction)
            |> Array.truncate 32
        let previousSnaps=anchorSnaps previousCoordinates
        let nextSnaps=anchorSnaps nextCoordinates
        match routeDetailed mode previousSnaps nextSnaps turnback with
        | RoutingUnavailable reason,_ ->
            { corridor={ availability="unavailable"; unavailableReason=Some reason
                         baselineCost=None; corridorId=None; ingressThreadId=None
                         egressThreadId=None; corridorEdges=[||]
                         alternativeCorridorCount=0; alternativeCorridorIds=[||]; alternativeCostGap=None }
              candidates=Array.create candidateCoordinates.Length (unavailableCandidate "baseline-unavailable") }
        | Routed _,None ->
            { corridor={ availability="unavailable"; unavailableReason=Some "baseline-unavailable"
                         baselineCost=None; corridorId=None; ingressThreadId=None
                         egressThreadId=None; corridorEdges=[||]
                         alternativeCorridorCount=0; alternativeCorridorIds=[||]; alternativeCostGap=None }
              candidates=Array.create candidateCoordinates.Length (unavailableCandidate "baseline-unavailable") }
        | Routed baseline,Some baselinePath ->
            let previousSnap=baselinePath.origin
            let nextSnap=baselinePath.target
            let differentPrevious =
                previousSnaps
                |> Array.filter (fun state ->
                    localFaceIdentity state.edgeId <> localFaceIdentity previousSnap.edgeId)
            let differentNext =
                nextSnaps
                |> Array.filter (fun state ->
                    localFaceIdentity state.edgeId <> localFaceIdentity nextSnap.edgeId)
            let ambiguityBand=max 25.0 (baseline*0.05)
            // Evidence capture deliberately extends beyond the current 500 m
            // publication policy. Policy replay can tighten the limit without
            // rebuilding the graph; it must never request more than this
            // captured horizon.
            let maximumUsefulLeg=baseline+PostInferenceEvidenceHorizonMetres
            let forwardExploration,reverseExploration =
                if turnback then None,None else
                let forward,parents,forwardLimit=exploreForward mode previousSnap maximumUsefulLeg
                let reverse,successors,reverseLimit=exploreReverse mode nextSnap maximumUsefulLeg
                Some(forward,parents,forwardLimit),Some(reverse,successors,reverseLimit)
            // Candidate explorations already contain the exact costs at each
            // alternative anchor edge. Re-run A* only for an alternative that
            // can actually fall inside the ambiguity band.
            let plausiblePrevious =
                match reverseExploration with
                | None -> differentPrevious
                | Some(reverse,_,_) ->
                    differentPrevious
                    |> Array.filter (fun state ->
                        match reverse.TryGetValue(state.edgeId) with
                        | true,distance ->
                            distance-state.fraction*edgeCost state.edgeId <= baseline+ambiguityBand
                        | _ -> false)
            let plausibleNext =
                match forwardExploration with
                | None -> differentNext
                | Some(forward,_,_) ->
                    differentNext
                    |> Array.filter (fun state ->
                        match forward.TryGetValue(state.edgeId) with
                        | true,distance ->
                            distance-(1.0-state.fraction)*edgeCost state.edgeId <= baseline+ambiguityBand
                        | _ -> false)
            let alternativePath (starts: DirectedSnapState array)
                                (finishes: DirectedSnapState array) =
                if starts.Length=0 || finishes.Length=0 then None
                else
                    match routeDetailed mode starts finishes turnback with
                    | Routed alternative,Some path -> Some(alternative,path)
                    | _ -> None
            let alternatives =
                [| alternativePath plausiblePrevious nextSnaps
                   alternativePath [|previousSnap|] plausibleNext
                   // Station loops and divided approaches commonly require a
                   // coordinated change at both ends. Neither one-ended route
                   // is necessarily competitive on its own.
                   alternativePath differentPrevious differentNext |]
                |> Array.choose id
                |> Array.filter (fun (_,path) ->
                    corridorIdentity path.edges<>corridorIdentity baselinePath.edges)
                |> Array.distinctBy (fun (_,path) -> corridorIdentity path.edges)
                |> Array.sortBy (fun (cost,path) -> cost,corridorIdentity path.edges)
            let tied=alternatives |> Array.filter (fun (cost,_) -> cost-baseline<=ambiguityBand) |> Array.truncate 2
            let alternativeGap=alternatives |> Array.map (fun (cost,_) -> cost-baseline) |> Array.sort |> Array.tryHead
            let corridor =
                let ingressThread = baselinePath.edges |> Array.tryHead |> Option.map directedThreadIdentity
                let egressThread = baselinePath.edges |> Array.tryLast |> Option.map directedThreadIdentity
                { availability=if tied.Length=0 then "available" else "ambiguous"
                  unavailableReason=None
                  baselineCost=Some baseline; corridorId=Some(corridorIdentity baselinePath.edges)
                  ingressThreadId=ingressThread; egressThreadId=egressThread
                  corridorEdges=baselinePath.edges; alternativeCorridorCount=1+tied.Length
                  alternativeCorridorIds=tied |> Array.map (fun (_,path) -> corridorIdentity path.edges)
                  alternativeCostGap=alternativeGap }
            let projectedCandidates =
                candidateCoordinates |> Array.map (fun struct(lon,lat) -> projected lon lat)
            let tiedFaceAgreement candidateX candidateY attachment =
                match candidateGeometry baselinePath.edges candidateX candidateY attachment with
                // An attachment on a distinct branch/bay is the evidence that
                // disambiguates otherwise tied candidate-independent anchor
                // corridors.  It cannot occur in every baseline by definition;
                // its own routed incoming/outgoing legs and excess still have
                // to pass.  Only candidates attached to the baseline itself
                // must retain the same face across all tied baselines.
                | None -> Some true
                | Some(_,_,_,_,baselineFace,_,_,_) ->
                    let agrees =
                        tied
                        |> Array.forall (fun (_,path) ->
                            match candidateGeometry path.edges candidateX candidateY attachment with
                            | Some(_,_,_,_,face,_,_,_) -> face=baselineFace
                            | None -> false)
                    Some agrees
            // A tied anchor route is candidate evidence, not a stop-wide veto.
            // In particular, an obsolete or remote observation must not force
            // every otherwise stable hypothesis to abstain.  Each candidate is
            // checked against every tied corridor below using its selected
            // fractional attachment state.
            begin
                if turnback then
                    let values =
                        projectedCandidates
                        |> parallelMap (fun struct(candidateX,candidateY) ->
                            let candidateSnaps = snap mode candidateX candidateY
                            if candidateSnaps.Length=0 then
                                unavailableCandidate "corridor-unavailable"
                            else
                            match routeDetailed mode [|previousSnap|] candidateSnaps false with
                            | Routed left, Some leftPath ->
                                let candidateSnap=leftPath.target
                                let geometry =
                                    candidateGeometry baselinePath.edges candidateX candidateY candidateSnap
                                    |> Option.orElseWith (fun () ->
                                        Some(candidateAttachmentGeometry candidateX candidateY candidateSnap))
                                match geometry with
                                | Some(distance,signed,corridorHeading,attachmentHeading,faceId,alignment,side,proximity) ->
                                    match route mode [|candidateSnap|] [|nextSnap|] true with
                                    | Routed right ->
                                        let excess=max 0.0 (left+right-baseline)
                                        if excess>PostInferenceEvidenceHorizonMetres then unavailableCandidate "evidence-horizon"
                                        else { routed=Routed excess; snap=Some candidateSnap
                                               corridorDistance=Some distance; signedLateralOffset=Some signed
                                               corridorHeading=Some corridorHeading; attachmentHeading=Some attachmentHeading
                                               localFaceId=Some faceId
                                               tiedCorridorsAgree=(tied.Length=0 || tiedFaceAgreement candidateX candidateY candidateSnap=Some true)
                                               alignment=alignment; side=side; proximity=proximity; rejectionReason=None }
                                    | RoutingUnavailable reason -> unavailableCandidate reason
                                | None -> unavailableCandidate "corridor-unavailable"
                            | RoutingUnavailable reason,_ -> unavailableCandidate reason
                            | Routed _,None -> unavailableCandidate "candidate-edge-unavailable")
                    { corridor=corridor; candidates=values }
                else
                    let candidateSnaps =
                        projectedCandidates
                        |> Array.map (fun struct(candidateX,candidateY) ->
                            snap mode candidateX candidateY)
                    let forward,forwardParents,forwardLimit = forwardExploration.Value
                    let reverse,reverseSuccessors,reverseLimit = reverseExploration.Value
                    let candidatePath state =
                        let before=ResizeArray<int>()
                        let mutable current=state.edgeId
                        before.Add(current)
                        while forwardParents.ContainsKey(current) do
                            current<-forwardParents.[current]
                            before.Add(current)
                        let after=ResizeArray<int>()
                        current<-state.edgeId
                        while reverseSuccessors.ContainsKey(current) do
                            current<-reverseSuccessors.[current]
                            after.Add(current)
                        Array.append (before |> Seq.rev |> Seq.toArray) (after.ToArray())
                    if forwardLimit || reverseLimit then
                        { corridor={corridor with availability="unavailable"; unavailableReason=Some "search-limit"}
                          candidates=Array.create candidateCoordinates.Length (unavailableCandidate "search-limit") }
                    else
                        let values =
                            Array.map2 (fun struct(candidateX,candidateY) snapsForCandidate ->
                                snapsForCandidate
                                |> Array.choose (fun state ->
                                    match forward.TryGetValue(state.edgeId),reverse.TryGetValue(state.edgeId) with
                                    | (true,leftEnd),(true,rightFrom) ->
                                        let left=leftEnd-(1.0-state.fraction)*edgeCost state.edgeId
                                        let right=rightFrom-state.fraction*edgeCost state.edgeId
                                        if left < -0.0001 || right < -0.0001 then None
                                        else Some(max 0.0 (max 0.0 left+max 0.0 right-baseline),state)
                                    | _ -> None)
                                |> Array.sortBy (fun (excess,state) ->
                                    excess,state.perpendicularDistance,state.edgeId,state.fraction)
                                |> function
                                    | [||] -> unavailableCandidate "routed-excess-or-disconnected"
                                    | candidates ->
                                        let excess,state=candidates.[0]
                                        if excess>PostInferenceEvidenceHorizonMetres then unavailableCandidate "evidence-horizon"
                                        else
                                            let geometry =
                                                candidateGeometry baselinePath.edges candidateX candidateY state
                                                |> Option.orElseWith (fun () ->
                                                    candidateGeometry (candidatePath state) candidateX candidateY state)
                                            match geometry with
                                            | Some(distance,signed,corridorHeading,attachmentHeading,faceId,alignment,side,proximity) ->
                                                { routed=Routed excess; snap=Some state; corridorDistance=Some distance
                                                  signedLateralOffset=Some signed; corridorHeading=Some corridorHeading
                                                  attachmentHeading=Some attachmentHeading; localFaceId=Some faceId
                                                  tiedCorridorsAgree=(tied.Length=0 || tiedFaceAgreement candidateX candidateY state=Some true)
                                                  alignment=alignment; side=side
                                                  proximity=proximity; rejectionReason=None }
                                            | None -> unavailableCandidate "corridor-unavailable")
                                projectedCandidates candidateSnaps
                        { corridor=corridor; candidates=values }
            end
    let evaluateBoundaryCandidates mode (anchorCoordinates: struct(float*float) array)
                                   (candidateCoordinates: struct(float*float) array)
                                   outgoingFromCandidate =
        let projected lon lat =
            let mutable x,y=lon,lat
            wgs84ToEtrs89Ex.Transform(&x,&y)
            struct(x,y)
        let anchorSnaps =
            anchorCoordinates
            |> Array.collect (fun struct(lon,lat) ->
                let struct(x,y)=projected lon lat
                snap mode x y |> Array.truncate 4)
            |> Array.distinctBy (fun state -> state.edgeId,state.fraction)
            |> Array.sortBy (fun state -> state.perpendicularDistance,state.edgeId,state.fraction)
            |> Array.truncate 32
        let projectedCandidates =
            candidateCoordinates
            |> Array.map (fun struct(lon,lat) ->
                let struct(candidateX,candidateY)=projected lon lat
                struct(candidateX,candidateY,snap mode candidateX candidateY))
        let superTargetSnaps =
            projectedCandidates
            |> Array.collect (fun struct(_,_,states) -> states)
            |> Array.distinctBy (fun state -> state.edgeId,state.fraction)
        let baselineResult =
            if outgoingFromCandidate then routeDetailed mode superTargetSnaps anchorSnaps false
            else routeDetailed mode anchorSnaps superTargetSnaps false
        match baselineResult with
        | RoutingUnavailable reason,_ ->
            let corridor={ availability="unavailable"; unavailableReason=Some reason; baselineCost=None
                           corridorId=None; ingressThreadId=None; egressThreadId=None
                           corridorEdges=[||]; alternativeCorridorCount=0
                           alternativeCorridorIds=[||]; alternativeCostGap=None }
            projectedCandidates
            |> Array.map (fun _ -> struct(corridor,unavailableCandidate reason))
        | Routed _,None ->
            let corridor={ availability="unavailable"; unavailableReason=Some "boundary-baseline-unavailable"
                           baselineCost=None; corridorId=None; ingressThreadId=None; egressThreadId=None
                           corridorEdges=[||]; alternativeCorridorCount=0
                           alternativeCorridorIds=[||]; alternativeCostGap=None }
            projectedCandidates
            |> Array.map (fun _ -> struct(corridor,unavailableCandidate "boundary-baseline-unavailable"))
        | Routed baseline,Some baselinePath ->
            let corridor = {
                availability="available"; unavailableReason=None; baselineCost=Some baseline
                corridorId=Some(corridorIdentity baselinePath.edges)
                ingressThreadId=baselinePath.edges |> Array.tryHead |> Option.map directedThreadIdentity
                egressThreadId=baselinePath.edges |> Array.tryLast |> Option.map directedThreadIdentity
                corridorEdges=baselinePath.edges
                alternativeCorridorCount=1; alternativeCorridorIds=[||]; alternativeCostGap=None }
            let maximumCost=baseline+PostInferenceEvidenceHorizonMetres
            let distances,_,limitReached =
                if outgoingFromCandidate then exploreReverse mode baselinePath.target maximumCost
                else exploreForward mode baselinePath.origin maximumCost
            if limitReached then
                projectedCandidates
                |> Array.map (fun _ ->
                    struct({corridor with availability="unavailable"; unavailableReason=Some "search-limit"},
                           unavailableCandidate "search-limit"))
            else
                projectedCandidates
                |> Array.map (fun struct(candidateX,candidateY,states) ->
                    let candidates =
                        states
                        |> Array.choose (fun state ->
                            match distances.TryGetValue(state.edgeId) with
                            | false,_ -> None
                            | true,distance ->
                                let cost =
                                    if outgoingFromCandidate then distance-state.fraction*edgeCost state.edgeId
                                    else distance-(1.0-state.fraction)*edgeCost state.edgeId
                                if cost < -0.0001 then None else Some(max 0.0 cost,state))
                        |> Array.sortBy (fun (cost,state) ->
                            cost,state.perpendicularDistance,state.edgeId,state.fraction)
                    match candidates with
                    | [||] -> struct(corridor,unavailableCandidate "routed-excess-or-disconnected")
                    | values ->
                        let cost,attachment=values.[0]
                        let excess=max 0.0(cost-baseline)
                        if excess>PostInferenceEvidenceHorizonMetres then struct(corridor,unavailableCandidate "evidence-horizon") else
                        let geometry =
                            candidateGeometry baselinePath.edges candidateX candidateY attachment
                            |> Option.orElseWith (fun () ->
                                Some(candidateAttachmentGeometry candidateX candidateY attachment))
                        match geometry with
                        | Some(distance,signed,corridorHeading,attachmentHeading,faceId,alignment,side,proximity) ->
                            struct(corridor,
                                { routed=Routed excess; snap=Some attachment
                                  corridorDistance=Some distance; signedLateralOffset=Some signed
                                  corridorHeading=Some corridorHeading; attachmentHeading=Some attachmentHeading
                                  localFaceId=Some faceId; tiedCorridorsAgree=false
                                  alignment=alignment; side=side; proximity=proximity; rejectionReason=None })
                        | None -> struct(corridor,unavailableCandidate "corridor-unavailable"))
    let routeViaMany mode previousLon previousLat candidateCoordinates nextLon nextLat turnback =
        (routeViaManyEvidence mode [|struct(previousLon,previousLat)|] candidateCoordinates
                                  [|struct(nextLon,nextLat)|] turnback).candidates
        |> Array.map (fun value -> value.routed)
    member _.NodeCount = nodeCount
    member _.EdgeCount = edgeCount
    member _.CacheEntries = cache.Count
    member _.CacheHits = cacheHits
    member _.CacheMisses = cacheMisses
    member _.Searches = Interlocked.Read(&searches)
    member _.RestrictionLookups = Interlocked.Read(&restrictionLookups)
    member _.RestrictionRulesExamined = Interlocked.Read(&restrictionRulesExamined)
    member _.PrepareKnownSnaps(coordinates: struct (float * float) array,
                               progress: int64 -> int64 option -> unit) =
        if disposed then raise (ObjectDisposedException(nameof PackedRoutingGraph))
        preparedSnaps.Clear()
        let projected = ResizeArray<struct (float * float)>()
        let coordinateIndexes = Dictionary<struct (int64 * int64), int>()
        for struct (lon,lat) in coordinates do
            let mutable x,y = lon,lat
            wgs84ToEtrs89Ex.Transform(&x,&y)
            let key = struct (
                int64 (Math.Round(x * 10.0, MidpointRounding.AwayFromZero)),
                int64 (Math.Round(y * 10.0, MidpointRounding.AwayFromZero)))
            if not (coordinateIndexes.ContainsKey(key)) then
                coordinateIndexes.[key] <- projected.Count
                projected.Add(struct (x,y))
        let queryCellSize = 200.0
        let queryCell value = int (Math.Floor(value/queryCellSize))
        let queryGrid = Dictionary<int64, ResizeArray<int>>()
        for index = 0 to projected.Count-1 do
            let struct (x,y) = projected.[index]
            let key = routingGridKey (queryCell x) (queryCell y)
            match queryGrid.TryGetValue(key) with
            | true, values -> values.Add(index)
            | _ ->
                let values = ResizeArray()
                values.Add(index)
                queryGrid.[key] <- values
        let slotsPerMode = 8
        let modesPerQuery = 3
        let slotCount = projected.Count*modesPerQuery*slotsPerMode
        let resultEdges = Array.create slotCount -1
        let resultDistances = Array.create slotCount Double.PositiveInfinity
        let resultFractions = Array.zeroCreate<float> slotCount
        let stripes = Array.init 2048 (fun _ -> obj())
        let addResult queryIndex modeIndex edge fraction distance =
            lock stripes.[queryIndex &&& (stripes.Length-1)] (fun () ->
                let first = (queryIndex*modesPerQuery+modeIndex)*slotsPerMode
                let mutable target, worst = -1, -1
                for slot = first to first+slotsPerMode-1 do
                    if resultEdges.[slot] < 0 && target < 0 then target <- slot
                    elif resultEdges.[slot] >= 0
                         && (worst < 0
                             || resultDistances.[slot] > resultDistances.[worst]
                             || (resultDistances.[slot] = resultDistances.[worst]
                                 && resultEdges.[slot] > resultEdges.[worst])) then
                        worst <- slot
                if target < 0 then target <- worst
                if target >= 0
                   && (resultEdges.[target] < 0
                       || distance < resultDistances.[target]
                       || (distance = resultDistances.[target] && edge < resultEdges.[target])) then
                    resultEdges.[target] <- edge
                    resultDistances.[target] <- distance
                    resultFractions.[target] <- fraction)
        let chunkSize = 250_000
        let chunks = (edgeCount+chunkSize-1)/chunkSize
        let mutable completedEdges = 0L
        let progressLock = obj()
        let mutable lastProgress = 0L
        let options = ParallelOptions(MaxDegreeOfParallelism = maximumWorkers)
        Parallel.For(0, chunks, options, fun chunk ->
            let firstEdge = chunk*chunkSize
            let finishEdge = min edgeCount (firstEdge+chunkSize)
            for edge = firstEdge to finishEdge-1 do
                let left,right = edgeFrom edge,edgeTo edge
                let x0,y0,x1,y1 = nodeX left,nodeY left,nodeX right,nodeY right
                let minCellX = queryCell (min x0 x1-75.0)
                let maxCellX = queryCell (max x0 x1+75.0)
                let minCellY = queryCell (min y0 y1-75.0)
                let maxCellY = queryCell (max y0 y1+75.0)
                for cellX = minCellX to maxCellX do
                    for cellY = minCellY to maxCellY do
                        match queryGrid.TryGetValue(routingGridKey cellX cellY) with
                        | true, queryIndexes ->
                            for queryIndex in queryIndexes do
                                let struct (px,py) = projected.[queryIndex]
                                let dx,dy = x1-x0,y1-y0
                                let lengthSquared = dx*dx+dy*dy
                                let fraction =
                                    if lengthSquared = 0.0 then 0.0
                                    else max 0.0 (min 1.0 (((px-x0)*dx+(py-y0)*dy)/lengthSquared))
                                let x,y = x0+fraction*dx,y0+fraction*dy
                                let distance = Math.Sqrt((px-x)**2.0+(py-y)**2.0)
                                if distance <= 75.0 then
                                    let flags = edgeFlags edge
                                    if flags &&& modeFlag RoadBus <> 0us then addResult queryIndex 0 edge fraction distance
                                    if flags &&& modeFlag Trolleybus <> 0us then addResult queryIndex 1 edge fraction distance
                                    if flags &&& modeFlag TramRouting <> 0us then addResult queryIndex 2 edge fraction distance
                        | _ -> ()
            let completed = Interlocked.Add(&completedEdges, int64 (finishEdge-firstEdge))
            lock progressLock (fun () ->
                if completed > lastProgress then
                    lastProgress <- completed
                    progress completed (Some (int64 edgeCount))))
        |> ignore
        for queryIndex = 0 to projected.Count-1 do
            let struct (x,y) = projected.[queryIndex]
            for modeIndex = 0 to modesPerQuery-1 do
                let first = (queryIndex*modesPerQuery+modeIndex)*slotsPerMode
                let values =
                    [| for slot = first to first+slotsPerMode-1 do
                           if resultEdges.[slot] >= 0 then
                               let edge=resultEdges.[slot]
                               let fraction=resultFractions.[slot]
                               let left,right=edgeFrom edge,edgeTo edge
                               let x=nodeX left+fraction*(nodeX right-nodeX left)
                               let y=nodeY left+fraction*(nodeY right-nodeY left)
                               yield { edgeId=edge; fraction=fraction; projectedX=x; projectedY=y
                                       perpendicularDistance=resultDistances.[slot] } |]
                    |> Array.sortBy (fun state -> state.perpendicularDistance,state.edgeId,state.fraction)
                    |> fun ordered ->
                        if ordered.Length=0 then ordered else
                        let maximumPlausibleDistance=ordered.[0].perpendicularDistance+15.0
                        ordered |> Array.filter (fun state -> state.perpendicularDistance<=maximumPlausibleDistance)
                let mode = if modeIndex = 0 then RoadBus elif modeIndex = 1 then Trolleybus else TramRouting
                preparedSnaps.[coordinateKey mode x y] <- values
        progress (int64 edgeCount) (Some (int64 edgeCount))
        Log.Information("Prepared routing snaps for {QueryCount} conversion coordinates", projected.Count)
    member _.MaximumWorkers
        with get () = maximumWorkers
        and set value =
            if value <= 0 then invalidArg "value" "Routing worker count must be positive"
            maximumWorkers <- value
    member _.EstimatedMappedBytes =
        int64 nodeCount*routingNodeStride + int64(nodeCount+1)*16L
        + int64 edgeCount*(routingEdgeStride+4L) + int64 snapCount*routingSnapStride
    member _.Route(mode, fromLon:float, fromLat:float, toLon:float, toLat:float, ?requireTransition:bool) =
        if disposed then raise (ObjectDisposedException(nameof PackedRoutingGraph))
        let mutable fromX,fromY = fromLon,fromLat
        let mutable toX,toY = toLon,toLat
        wgs84ToEtrs89Ex.Transform(&fromX,&fromY)
        wgs84ToEtrs89Ex.Transform(&toX,&toY)
        let q value = int (Math.Round(value/5.0, MidpointRounding.AwayFromZero))
        let modeKey = match mode with RoadBus -> 0uy | Trolleybus -> 1uy | TramRouting -> 2uy
        let required = defaultArg requireTransition false
        let key = struct(modeKey,q fromX,q fromY,q toX,q toY,required)
        match cache.TryGetValue(key) with
        | true, result -> Interlocked.Increment(&cacheHits) |> ignore; result
        | _ ->
            Interlocked.Increment(&cacheMisses) |> ignore
            let result = route mode (snap mode fromX fromY) (snap mode toX toY) required
            let mutable oldest = Unchecked.defaultof<struct (byte * int * int * int * int * bool)>
            let mutable ignored = Unchecked.defaultof<RoutedDistance>
            while cache.Count >= 250_000 && cacheOrder.TryDequeue(&oldest) do
                cache.TryRemove(oldest, &ignored) |> ignore
            if cache.TryAdd(key, result) then cacheOrder.Enqueue(key)
            match cache.TryGetValue(key) with
            | true, stored -> stored
            | _ -> result
    member _.RouteViaMany(mode, previousLon, previousLat,
                          candidateCoordinates: struct(float*float) array,
                          nextLon, nextLat, ?requireTurnback) =
        routeViaMany mode previousLon previousLat candidateCoordinates nextLon nextLat
                     (defaultArg requireTurnback false)
    member _.EvaluateCandidates(mode, previousLon, previousLat,
                                candidateCoordinates: struct(float*float) array,
                                nextLon, nextLat, ?requireTurnback) =
        routeViaManyEvidence mode [|struct(previousLon,previousLat)|] candidateCoordinates
                             [|struct(nextLon,nextLat)|] (defaultArg requireTurnback false)
    member _.EvaluateCandidatesWithAnchors(mode,
                                           previousCoordinates: struct(float*float) array,
                                           candidateCoordinates: struct(float*float) array,
                                           nextCoordinates: struct(float*float) array,
                                           ?requireTurnback) =
        routeViaManyEvidence mode previousCoordinates candidateCoordinates nextCoordinates
                             (defaultArg requireTurnback false)
    member _.CaptureContextEvidence(mode,
                                    previousCoordinates: struct(float*float) array,
                                    routePointCoordinates: struct(float*float) array,
                                    nextCoordinates: struct(float*float) array,
                                    ?requireTurnback) =
        if disposed then raise(ObjectDisposedException(nameof PackedRoutingGraph))
        routeContextEvidence mode previousCoordinates routePointCoordinates nextCoordinates
                             (defaultArg requireTurnback false)
    member _.EvaluateBoundaryCandidates(mode, anchorLon, anchorLat,
                                        candidateCoordinates: struct(float*float) array,
                                        outgoingFromCandidate) =
        evaluateBoundaryCandidates mode [|struct(anchorLon,anchorLat)|]
                                   candidateCoordinates outgoingFromCandidate
    member _.EvaluateBoundaryCandidatesWithAnchors(mode,
                                                   anchorCoordinates: struct(float*float) array,
                                                   candidateCoordinates: struct(float*float) array,
                                                   outgoingFromCandidate) =
        evaluateBoundaryCandidates mode anchorCoordinates candidateCoordinates outgoingFromCandidate
    member _.InspectSnaps(mode, lon, lat) =
        let mutable x,y=lon,lat
        wgs84ToEtrs89Ex.Transform(&x,&y)
        snap mode x y
        |> Array.map (fun state ->
            let mutable projectedLon,projectedLat=state.projectedX,state.projectedY
            etrs89ExToWgs84.Transform(&projectedLon,&projectedLat)
            { snapEdgeId=state.edgeId; wayId=edgeWay state.edgeId; snapFraction=state.fraction
              projectedLon=projectedLon; projectedLat=projectedLat
              snapDistance=state.perpendicularDistance
              localThreadId=directedThreadIdentity state.edgeId })
    member _.RouteVia(mode, previousLon, previousLat, candidateLon, candidateLat,
                      nextLon, nextLat, ?requireTurnback) =
        routeViaMany mode previousLon previousLat [| struct(candidateLon,candidateLat) |]
                     nextLon nextLat (defaultArg requireTurnback false)
        |> Array.head
    interface IDisposable with
        member _.Dispose() =
            if not disposed then
                disposed <- true
                searchWorkspaces.Dispose()
                snaps.SafeMemoryMappedViewHandle.ReleasePointer()
                reverseEdges.SafeMemoryMappedViewHandle.ReleasePointer()
                reverseOffsets.SafeMemoryMappedViewHandle.ReleasePointer()
                edges.SafeMemoryMappedViewHandle.ReleasePointer()
                offsets.SafeMemoryMappedViewHandle.ReleasePointer()
                nodes.SafeMemoryMappedViewHandle.ReleasePointer()
                snaps.Dispose(); snapMap.Dispose(); edges.Dispose(); edgeMap.Dispose()
                reverseEdges.Dispose(); reverseEdgeMap.Dispose()
                reverseOffsets.Dispose(); reverseOffsetMap.Dispose()
                offsets.Dispose(); offsetMap.Dispose(); nodes.Dispose(); nodeMap.Dispose()
                deleteTemporaryDirectory ()
    static member OpenWithProgress(pbfPath:string,
                                   progress: string -> int64 -> int64 option -> unit,
                                   ?buildGlobalSnaps: bool) =
        let buildGlobalSnaps = defaultArg buildGlobalSnaps true
        let pbfPath = Path.GetFullPath(pbfPath)
        if not (File.Exists(pbfPath)) then invalidArg "pbfPath" $"Routing PBF is missing: {pbfPath}"
        let tempDirectory = Path.Combine(Path.GetTempPath(),$"jrutil-routing-{Guid.NewGuid():N}")
        Directory.CreateDirectory(tempDirectory) |> ignore
        try
            let createFile name length =
                let path=Path.Combine(tempDirectory,name)
                use file=new FileStream(path,FileMode.CreateNew,FileAccess.ReadWrite,FileShare.Read)
                file.SetLength(max 1L length); path
            let openMap path =
                let map=MemoryMappedFile.CreateFromFile(path,FileMode.Open,null,0L,MemoryMappedFileAccess.ReadWrite)
                map,map.CreateViewAccessor(0L,0L,MemoryMappedFileAccess.ReadWrite)
            let mutable nodeValues = ResizeArray<RoutingNodeBuild>()
            let edgeSpoolPath = Path.Combine(tempDirectory,"edges.spool")
            use edgeSpool =
                new FileStream(edgeSpoolPath,FileMode.CreateNew,FileAccess.ReadWrite,FileShare.Read,
                               1_048_576,FileOptions.SequentialScan ||| FileOptions.DeleteOnClose)
            use edgeWriter = new BinaryWriter(edgeSpool,Encoding.UTF8,true)
            let mutable edgeValueCount = 0
            let mutable outgoingDegrees: int64 array = [||]
            let mutable incomingDegrees: int64 array = [||]
            let restrictionValues = ResizeArray<RoutingRestriction>()
            use wayAudit =
                new StreamWriter(Path.Combine(tempDirectory,"way-audit.tsv"),false,UTF8Encoding(false))
            let mutable lastNodeId = Int64.MinValue
            let mutable nextEdgeProgress = 1_000_000
            let mutable packedNodes: (MemoryMappedFile * MemoryMappedViewAccessor) option = None
            let mutable packedNodeCount = 0
            let ensurePackedNodes () =
                match packedNodes with
                | Some value -> value
                | None ->
                    packedNodeCount <- nodeValues.Count
                    let nodeMap,nodes =
                        openMap(createFile "nodes.bin" (int64 packedNodeCount*routingNodeStride))
                    for i=0 to packedNodeCount-1 do
                        let v,p=nodeValues.[i],int64 i*routingNodeStride
                        nodes.Write(p,v.id);nodes.Write(p+8L,v.x);nodes.Write(p+12L,v.y)
                        nodes.Write(p+16L,v.barrierBlocked)
                    nodes.Flush()
                    packedNodes <- Some(nodeMap,nodes)
                    nodeValues <- ResizeArray<RoutingNodeBuild>()
                    outgoingDegrees <- Array.zeroCreate packedNodeCount
                    incomingDegrees <- Array.zeroCreate packedNodeCount
                    GCSettings.LargeObjectHeapCompactionMode <- GCLargeObjectHeapCompactionMode.CompactOnce
                    GC.Collect(2, GCCollectionMode.Forced, true, true)
                    nodeMap,nodes
            let addEdge (value: RoutingEdgeBuild) =
                edgeWriter.Write(value.fromNode)
                edgeWriter.Write(value.toNode)
                edgeWriter.Write(value.length)
                edgeWriter.Write(value.wayId)
                edgeWriter.Write(value.flags)
                outgoingDegrees.[value.fromNode] <- outgoingDegrees.[value.fromNode] + 1L
                incomingDegrees.[value.toNode] <- incomingDegrees.[value.toNode] + 1L
                edgeValueCount <- edgeValueCount + 1
            let nodeBuildId index =
                match packedNodes with
                | Some (_,nodes) -> nodes.ReadInt64(int64 index*routingNodeStride)
                | None -> nodeValues.[index].id
            let nodeBuildX index =
                match packedNodes with
                | Some (_,nodes) -> float (nodes.ReadSingle(int64 index*routingNodeStride+8L))
                | None -> float nodeValues.[index].x
            let nodeBuildY index =
                match packedNodes with
                | Some (_,nodes) -> float (nodes.ReadSingle(int64 index*routingNodeStride+12L))
                | None -> float nodeValues.[index].y
            let nodeBuildBlocked index =
                match packedNodes with
                | Some (_,nodes) -> nodes.ReadByte(int64 index*routingNodeStride+16L) <> 0uy
                | None -> nodeValues.[index].barrierBlocked <> 0uy
            let tryNode nodeId =
                let count = if packedNodes.IsSome then packedNodeCount else nodeValues.Count
                let mutable low,high,found = 0,count-1,-1
                while low <= high && found < 0 do
                    let middle = low+(high-low)/2
                    let middleId = nodeBuildId middle
                    if middleId = nodeId then found <- middle
                    elif middleId < nodeId then low <- middle+1 else high <- middle-1
                found
            use stream = File.OpenRead(pbfPath)
            for osm in new PBFOsmStreamSource(stream) do
                match osm.Type with
                | OsmGeoType.Node ->
                    let node = osm :?> Node
                    if node.Id.HasValue && node.Latitude.HasValue && node.Longitude.HasValue then
                        if node.Id.Value < lastNodeId then invalidArg "pbfPath" "Routing PBF is not ID ordered"
                        lastNodeId <- node.Id.Value
                        let mutable x, y = node.Longitude.Value, node.Latitude.Value
                        wgs84ToEtrs89Ex.Transform(&x, &y)
                        nodeValues.Add({id=node.Id.Value;x=float32 x;y=float32 y
                                        barrierBlocked=if barrierBlocked node then 1uy else 0uy})
                        if nodeValues.Count % 1_000_000 = 0 then
                            progress "graph-nodes" (int64 nodeValues.Count) None
                | OsmGeoType.Way ->
                    ensurePackedNodes () |> ignore
                    let way = osm :?> Way
                    let flags = roadRoutingFlags way ||| tramRoutingFlags way
                    if flags <> 0us && way.Id.HasValue && not(isNull way.Nodes) then
                        let auditedTags =
                            [ "highway"; "railway"; "access"; "vehicle"; "motor_vehicle"
                              "bus"; "psv"; "oneway"; "junction"; "construction"
                              "abandoned"; "disused"; "proposed"; "access:conditional"
                              "vehicle:conditional"; "motor_vehicle:conditional"
                              "bus:conditional"; "psv:conditional" ]
                            |> List.choose (fun key -> wayTagOpt key way |> Option.map (fun value -> $"{key}={value}"))
                            |> String.concat ";"
                        wayAudit.WriteLine($"{way.Id.Value}\t{flags}\t{auditedTags}")
                        let forward,backward = routingDirections way
                        let mutable left = if way.Nodes.Length = 0 then -1 else tryNode way.Nodes.[0]
                        for index = 0 to way.Nodes.Length-2 do
                            let right = tryNode way.Nodes.[index+1]
                            if left >= 0 && right >= 0 && left <> right
                               && not (nodeBuildBlocked left)
                               && not (nodeBuildBlocked right) then
                                let dx = nodeBuildX right-nodeBuildX left
                                let dy = nodeBuildY right-nodeBuildY left
                                let length = float32(Math.Sqrt(dx*dx+dy*dy))
                                if forward then addEdge {fromNode=left;toNode=right;length=length;wayId=way.Id.Value;flags=flags}
                                if backward then addEdge {fromNode=right;toNode=left;length=length;wayId=way.Id.Value;flags=flags}
                            left <- right
                        if edgeValueCount >= nextEdgeProgress then
                            progress "graph-edges" (int64 edgeValueCount) None
                            nextEdgeProgress <- nextEdgeProgress + 1_000_000
                | OsmGeoType.Relation ->
                    let relation = osm :?> Relation
                    let tag name =
                        let mutable value = null
                        if not (isNull relation.Tags) && relation.Tags.TryGetValue(name, &value)
                        then Option.ofObj value else None
                    match tag "restriction" |> normalizedTag with
                    | Some value when not (isNull relation.Members) ->
                        let memberByRole role =
                            relation.Members |> Array.tryFind (fun relationMember -> relationMember.Role = role)
                        match memberByRole "from", memberByRole "to", memberByRole "via" with
                        | Some fromMember, Some toMember, Some viaMember
                            when fromMember.Type = OsmGeoType.Way
                                 && toMember.Type = OsmGeoType.Way
                                 && viaMember.Type = OsmGeoType.Node ->
                            let viaNode = tryNode viaMember.Id
                            if viaNode >= 0 then
                                restrictionValues.Add({fromWay=fromMember.Id;toWay=toMember.Id;viaNode=viaNode
                                                       only=value.StartsWith("only_",StringComparison.Ordinal)})
                        | _ -> ()
                    | _ -> ()
                | _ -> ()
            edgeWriter.Flush()
            progress "graph-edges" (int64 edgeValueCount) (Some (int64 edgeValueCount))
            progress "graph-csr" 0L (Some (int64 edgeValueCount))
            let nodeMap,nodes = ensurePackedNodes ()
            let nodeCount, edgeCount = packedNodeCount, edgeValueCount
            let mutable offsetValues = Array.zeroCreate<int64>(nodeCount+1)
            for index = 0 to outgoingDegrees.Length-1 do
                offsetValues.[index+1] <- outgoingDegrees.[index]
            outgoingDegrees <- Array.empty
            for index=1 to offsetValues.Length-1 do offsetValues.[index] <- offsetValues.[index]+offsetValues.[index-1]
            let offsetMap,offsets=openMap(createFile "offsets.bin" (int64 offsetValues.Length*8L))
            for i=0 to offsetValues.Length-1 do offsets.Write(int64 i*8L,offsetValues.[i])
            let edgeMap,edges=openMap(createFile "edges.bin" (int64 edgeCount*routingEdgeStride))
            // Counting scatter produces deterministic CSR directly from the
            // type/ID-ordered PBF stream. A nationwide comparison sort is both
            // unnecessary and one of the old builder's dominant CPU costs.
            let cursors = Array.copy offsetValues
            edgeSpool.Position <- 0L
            use edgeReader = new BinaryReader(edgeSpool,Encoding.UTF8,true)
            for i=0 to edgeCount-1 do
                let fromNode = edgeReader.ReadInt32()
                let toNode = edgeReader.ReadInt32()
                let length = edgeReader.ReadSingle()
                let wayId = edgeReader.ReadInt64()
                let flags = edgeReader.ReadUInt16()
                let target = int cursors.[fromNode]
                cursors.[fromNode] <- cursors.[fromNode] + 1L
                let p = int64 target*routingEdgeStride
                edges.Write(p,fromNode);edges.Write(p+4L,toNode);edges.Write(p+8L,length)
                edges.Write(p+12L,wayId);edges.Write(p+20L,flags)
                if (i + 1) % 1_000_000 = 0 then
                    progress "graph-csr" (int64 (i + 1)) (Some (int64 edgeCount))
            progress "graph-csr" (int64 edgeCount) (Some (int64 edgeCount))
            nodes.Flush(); offsets.Flush(); edges.Flush()
            progress "graph-reverse" 0L (Some (int64 edgeCount))
            let reverseOffsetValues = Array.zeroCreate<int64>(nodeCount+1)
            for index = 0 to incomingDegrees.Length-1 do
                reverseOffsetValues.[index+1] <- incomingDegrees.[index]
            incomingDegrees <- Array.empty
            for index=1 to reverseOffsetValues.Length-1 do
                reverseOffsetValues.[index] <- reverseOffsetValues.[index]+reverseOffsetValues.[index-1]
            let reverseOffsetMap,reverseOffsets =
                openMap(createFile "reverse-offsets.bin" (int64 reverseOffsetValues.Length*8L))
            for index=0 to reverseOffsetValues.Length-1 do
                reverseOffsets.Write(int64 index*8L,reverseOffsetValues.[index])
            let reverseEdgeMap,reverseEdges =
                openMap(createFile "reverse-edges.bin" (int64 edgeCount*4L))
            let reverseCursors = Array.copy reverseOffsetValues
            for edge=0 to edgeCount-1 do
                let destination = edges.ReadInt32(int64 edge*routingEdgeStride+4L)
                let target = reverseCursors.[destination]
                reverseCursors.[destination] <- target+1L
                reverseEdges.Write(target*4L,edge)
                if (edge+1) % 1_000_000 = 0 then
                    progress "graph-reverse" (int64 (edge+1)) (Some (int64 edgeCount))
            reverseOffsets.Flush(); reverseEdges.Flush()
            progress "graph-reverse" (int64 edgeCount) (Some (int64 edgeCount))
            edgeReader.Dispose()
            edgeWriter.Dispose()

            // From here on the packed maps are authoritative. Releasing the managed
            // build arrays before producing snaps avoids retaining national nodes,
            // edges, offsets and the snap index at the same time.
            nodeValues <- ResizeArray<RoutingNodeBuild>()
            offsetValues <- Array.empty
            GCSettings.LargeObjectHeapCompactionMode <- GCLargeObjectHeapCompactionMode.CompactOnce
            GC.Collect(2, GCCollectionMode.Forced, true, true)

            let snapValues = ResizeArray<SnapBuild>()
            if buildGlobalSnaps then
                for index = 0 to edgeCount-1 do
                    let edgePosition = int64 index*routingEdgeStride
                    let fromNode = edges.ReadInt32(edgePosition)
                    let toNode = edges.ReadInt32(edgePosition+4L)
                    let edgeLength = float (edges.ReadSingle(edgePosition+8L))
                    let fromPosition = int64 fromNode*routingNodeStride
                    let toPosition = int64 toNode*routingNodeStride
                    let leftX = float (nodes.ReadSingle(fromPosition+8L))
                    let leftY = float (nodes.ReadSingle(fromPosition+12L))
                    let rightX = float (nodes.ReadSingle(toPosition+8L))
                    let rightY = float (nodes.ReadSingle(toPosition+12L))
                    let steps = max 1 (int(Math.Ceiling(edgeLength/routingCellSize)))
                    let mutable lastKey = Int64.MinValue
                    for step=0 to steps do
                        let fraction=float step/float steps
                        let x=leftX+(rightX-leftX)*fraction
                        let y=leftY+(rightY-leftY)*fraction
                        let key = routingGridKey(routingGridCoordinate x)(routingGridCoordinate y)
                        if key <> lastKey then
                            snapValues.Add({key=key;edge=index})
                            lastKey <- key
                    if (index + 1) % 1_000_000 = 0 then
                        progress "graph-snaps" (int64 (index + 1)) (Some (int64 edgeCount))
                progress "graph-snaps" (int64 edgeCount) (Some (int64 edgeCount))
            snapValues.Sort(Comparer<SnapBuild>.Create(fun left right ->
                compare (left.key,left.edge) (right.key,right.edge)))
            let mutable uniqueSnapCount = 0
            for index = 0 to snapValues.Count-1 do
                if index = 0 || snapValues.[index].key <> snapValues.[index-1].key
                             || snapValues.[index].edge <> snapValues.[index-1].edge then
                    uniqueSnapCount <- uniqueSnapCount + 1
            let snapMap,snaps=openMap(createFile "snaps.bin" (int64 uniqueSnapCount*routingSnapStride))
            let mutable snapIndex = 0
            for i=0 to snapValues.Count-1 do
                let v = snapValues.[i]
                if i = 0 || v.key <> snapValues.[i-1].key || v.edge <> snapValues.[i-1].edge then
                    let p = int64 snapIndex*routingSnapStride
                    snaps.Write(p,v.key);snaps.Write(p+8L,v.edge)
                    snapIndex <- snapIndex + 1
            snaps.Flush()
            Log.Information("Packed routing graph ready: {NodeCount} nodes, {EdgeCount} directed edges",nodeCount,edgeCount)
            new PackedRoutingGraph(tempDirectory,nodeCount,edgeCount,uniqueSnapCount,
                                   nodeMap,nodes,offsetMap,offsets,edgeMap,edges,
                                   reverseOffsetMap,reverseOffsets,reverseEdgeMap,reverseEdges,
                                   snapMap,snaps,restrictionValues.ToArray())
        with _ ->
            try Directory.Delete(tempDirectory,true) with _ -> ()
            reraise()
    static member Open(pbfPath:string) =
        PackedRoutingGraph.OpenWithProgress(pbfPath, fun _ _ _ -> ())
