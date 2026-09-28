// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Learned stop-post scorer (port of jrutil/scripts/post-scorer). Two
/// conditional-logit stages with a "no correct candidate" option: stage 1
/// scores each context alone, stage 2 adds what related contexts at the stop
/// believe (reverse movement, same movement, same next/previous neighbour).
/// Decisions are per physical post, with an area fallback. All arithmetic is
/// deterministic: candidates are ordered by hypothesis id, reductions are
/// sequential.
module JrUtil.JdfPostScorer

open System
open System.Collections.Generic
open System.Text.Json

[<Literal>]
let ModelFormat = "post-scorer-two-stage-v1"

[<Literal>]
let StageFormat = "post-scorer-conditional-logit-v1"

[<Literal>]
let ComplexStopPosts = 3

let CollectiveColumns = [|
    "reverse_mass"; "has_reverse"; "same_movement_mass"; "has_same_movement"
    "same_next_mass"; "same_previous_mass" |]

type Stage = {
    features: string array
    mean: float array
    scale: float array
    weights: float array
    nullUtility: float
}

type Thresholds = { simple: float; complexStop: float; area: float option }

type Model = { stage1: Stage; stage2: Stage; thresholds: Thresholds }

/// One candidate of one context, with the stage-1 feature values in
/// `model.stage1.features` order.
type Candidate = {
    contextId: string
    hypothesisId: string
    postId: string
    areaId: string
    isPlatform: bool
    features: float array
}

/// Movement keys of a context. Missing neighbours are None.
type ContextKey = {
    contextId: string
    previousStop: string option
    nextStop: string option
    stopPostCount: int
}

type Resolution = Physical | Area | Abstain

type Decision = {
    contextId: string
    resolution: Resolution
    hypothesisId: string option
    postProbability: float
    areaProbability: float
}

[<Literal>]
let MaximumPostDiameterMetres = 25.0

[<Literal>]
let MaximumAreaDiameterMetres = 40.0

let private distances (latitudes: float array) (longitudes: float array) =
    let count = latitudes.Length
    let meanLatitude = if count = 0 then 50.0 else Array.average latitudes
    let cosine = Math.Cos(meanLatitude * Math.PI / 180.0)
    let y = latitudes |> Array.map (fun value -> value * 110_540.0)
    let x = longitudes |> Array.map (fun value -> value * 111_320.0 * cosine)
    Array2D.init count count (fun i j ->
        let dx = x.[i] - x.[j]
        let dy = y.[i] - y.[j]
        Math.Sqrt(dx * dx + dy * dy))

/// Closest-first complete-linkage clustering; returns a cluster index per input.
/// `mergeable` sees both clusters' member indexes. Ties go to the lower key
/// (diameter, then `tieBreak` of the union) exactly as the Python reference.
let private completeLinkage (distance: float[,]) maximumDiameter
                            (mergeable: Set<int> -> Set<int> -> bool) (useMemberTieBreak: bool) =
    let count = Array2D.length1 distance
    let clusters = ResizeArray<Set<int>>([ for index in 0 .. count - 1 -> Set.singleton index ])
    let mutable merging = true
    while merging do
        let mutable best: (float * int * int * int) option = None
        for left in 0 .. clusters.Count - 1 do
            for right in left + 1 .. clusters.Count - 1 do
                if mergeable clusters.[left] clusters.[right] then
                    let mutable diameter = 0.0
                    for i in clusters.[left] do
                        for j in clusters.[right] do
                            if distance.[i, j] > diameter then diameter <- distance.[i, j]
                    if diameter <= maximumDiameter then
                        let tie = if useMemberTieBreak then min (Set.minElement clusters.[left]) (Set.minElement clusters.[right]) else 0
                        match best with
                        | Some(bestDiameter, bestTie, _, _)
                            when not (diameter < bestDiameter || (diameter = bestDiameter && tie < bestTie)) -> ()
                        | _ -> best <- Some(diameter, tie, left, right)
        match best with
        | Some(_, _, left, right) ->
            clusters.[left] <- Set.union clusters.[left] clusters.[right]
            clusters.RemoveAt(right)
        | None -> merging <- false
    let assignment = Array.zeroCreate count
    for index in 0 .. clusters.Count - 1 do
        for mem in clusters.[index] do assignment.[mem] <- index
    assignment

/// Physical posts at one stop: a post never contains one source twice (a source maps
/// each post once), differing local_ref values never merge, diameter <= 25 m.
/// Inputs must be ordered by hypothesis id (post_scorer.features._cluster_stop).
let clusterPosts (latitudes: float array) (longitudes: float array)
                 (sources: string array array) (localRefs: string option array) =
    let sourceSets = sources |> Array.map Set.ofArray
    let refs = localRefs |> Array.map (function Some value -> Set.singleton value | None -> Set.empty)
    let union (members: Set<int>) (sets: Set<string> array) = members |> Seq.map (fun i -> sets.[i]) |> Set.unionMany
    completeLinkage (distances latitudes longitudes) MaximumPostDiameterMetres
        (fun left right ->
            Set.isEmpty (Set.intersect (union left sourceSets) (union right sourceSets))
            && Set.count (Set.union (union left refs) (union right refs)) <= 1)
        true

/// Areas of posts within 40 m that never mix tram and road posts ("any" joins either).
/// Inputs are post centres ordered by post id (post_scorer.features._cluster_areas).
let clusterAreas (latitudes: float array) (longitudes: float array) (modeClasses: string array) =
    let classes = modeClasses |> Array.map (fun value -> if value = "any" then Set.empty else Set.singleton value)
    let union (members: Set<int>) = members |> Seq.map (fun i -> classes.[i]) |> Set.unionMany
    completeLinkage (distances latitudes longitudes) MaximumAreaDiameterMetres
        (fun left right -> Set.count (Set.union (union left) (union right)) <= 1)
        false

let private parseStage (element: JsonElement) =
    let format = element.GetProperty("format").GetString()
    if format <> StageFormat then invalidArg "model" $"Unsupported scorer stage format: {format}"
    let floats (name: string) = element.GetProperty(name).EnumerateArray() |> Seq.map _.GetDouble() |> Seq.toArray
    let stage = {
        features = element.GetProperty("features").EnumerateArray() |> Seq.map _.GetString() |> Seq.toArray
        mean = floats "mean"; scale = floats "scale"; weights = floats "weights"
        nullUtility = element.GetProperty("null_utility").GetDouble() }
    let count = stage.features.Length
    if count = 0 || stage.mean.Length <> count || stage.scale.Length <> count || stage.weights.Length <> count then
        invalidArg "model" "Scorer stage vectors must match its feature list"
    if (Array.concat [| stage.mean; stage.scale; stage.weights; [| stage.nullUtility |] |]
        |> Array.exists (Double.IsFinite >> not))
       || stage.scale |> Array.exists (fun value -> value <= 0.0) then
        invalidArg "model" "Scorer stage parameters must be finite with positive scales"
    if stage.features |> Array.distinct |> Array.length <> count then
        invalidArg "model" "Scorer stage features must be distinct"
    stage

/// Parse the Python model.json (post-scorer-two-stage-v1).
let parseModel (element: JsonElement) =
    let format = element.GetProperty("format").GetString()
    if format <> ModelFormat then invalidArg "model" $"Unsupported scorer model format: {format}"
    let stage1 = parseStage (element.GetProperty("stage1"))
    let stage2 = parseStage (element.GetProperty("stage2"))
    let expected = Array.append stage1.features CollectiveColumns
    if stage2.features <> expected then
        invalidArg "model" "Scorer stage 2 must use the stage 1 features followed by the collective features"
    let thresholds = element.GetProperty("thresholds")
    let threshold (name: string) =
        let value = thresholds.GetProperty(name).GetDouble()
        if not (Double.IsFinite value) || value < 0.0 then invalidArg "model" $"Invalid scorer threshold {name}"
        value
    let area =
        match thresholds.TryGetProperty("area") with
        | true, value when value.ValueKind = JsonValueKind.Number -> Some(threshold "area")
        | _ -> None
    { stage1 = stage1; stage2 = stage2
      thresholds = { simple = threshold "simple"; complexStop = threshold "complex"; area = area } }

/// Softmax over one context's candidates plus the null option.
let private softmax (stage: Stage) (rows: float array array) =
    let utilities =
        rows |> Array.map (fun values ->
            let mutable total = 0.0
            for index = 0 to values.Length - 1 do
                total <- total + (values.[index] - stage.mean.[index]) / stage.scale.[index] * stage.weights.[index]
            total)
    let maximum = Array.fold max stage.nullUtility utilities
    let exponentials = utilities |> Array.map (fun value -> Math.Exp(value - maximum))
    let nullExponential = Math.Exp(stage.nullUtility - maximum)
    let mutable denominator = nullExponential
    for value in exponentials do denominator <- denominator + value
    exponentials |> Array.map (fun value -> value / denominator), nullExponential / denominator

let private noneKey = "∅"

/// Collective features for every (context, post) at one stop from stage-1 probabilities.
let collectiveFeatures (contexts: ContextKey array) (posts: (string * string * float) array) =
    // posts: (contextId, postId, post mass of the context on that post)
    let keyOf = contexts |> Array.map (fun c -> c.contextId, c) |> dict
    let movement (c: ContextKey) = (defaultArg c.previousStop noneKey, defaultArg c.nextStop noneKey)
    let mass = posts |> Array.map (fun (context, post, value) -> (context, post), value) |> dict
    let contextsBy (selector: ContextKey -> string) =
        contexts |> Array.groupBy selector |> dict
    let byMovement = contexts |> Array.groupBy movement |> dict
    let total (members: ContextKey array) post =
        let mutable sum = 0.0
        for c in members |> Array.sortBy _.contextId do
            match mass.TryGetValue((c.contextId, post)) with
            | true, value -> sum <- sum + value
            | _ -> ()
        sum
    let shared (selector: ContextKey -> string) =
        let groups = contextsBy selector
        fun (c: ContextKey) post own ->
            let key = selector c
            if key = noneKey then 0.0 else
            let members = groups.[key]
            if members.Length > 1 then (total members post - own) / float (members.Length - 1) else 0.0
    let sameNext = shared (fun c -> defaultArg c.nextStop noneKey)
    let samePrevious = shared (fun c -> defaultArg c.previousStop noneKey)
    let result = Dictionary<string * string, float array>()
    for (contextId, post, own) in posts do
        let c = keyOf.[contextId]
        let previous, next = movement c
        let members = byMovement.[(previous, next)]
        let sameMovement, hasSame =
            if members.Length > 1 then (total members post - own) / float (members.Length - 1), 1.0 else 0.0, 0.0
        let reverseMass, hasReverse =
            if previous = next then 0.0, 0.0 else
            match byMovement.TryGetValue((next, previous)) with
            | true, reverse -> total reverse post / float reverse.Length, 1.0
            | _ -> 0.0, 0.0
        result.[(contextId, post)] <-
            [| reverseMass; hasReverse; sameMovement; hasSame; sameNext c post own; samePrevious c post own |]
    result

let private postMass (candidates: Candidate array) (probabilities: float array) =
    let byPost = Dictionary<string * string, float>()
    for index = 0 to candidates.Length - 1 do
        let key = candidates.[index].contextId, candidates.[index].postId
        byPost.[key] <- (match byPost.TryGetValue key with | true, value -> value | _ -> 0.0) + probabilities.[index]
    byPost

/// Stage-1 and stage-2 probabilities of every candidate of one stop, in
/// (context id, hypothesis id) order.
let stopProbabilities (model: Model) (contexts: ContextKey array) (candidates: Candidate array) =
    let candidates = candidates |> Array.sortBy (fun c -> c.contextId, c.hypothesisId)
    let byContext = candidates |> Array.groupBy _.contextId
    let stage1 =
        [| for _, rows in byContext do
               let probabilities, _ = softmax model.stage1 (rows |> Array.map _.features)
               yield! probabilities |]
    let firstMass = postMass candidates stage1
    let collective =
        collectiveFeatures contexts
            (firstMass |> Seq.map (fun pair -> fst pair.Key, snd pair.Key, pair.Value)
                       |> Seq.sortBy (fun (context, post, _) -> context, post) |> Seq.toArray)
    let secondInputs =
        candidates |> Array.map (fun c -> Array.append c.features collective.[(c.contextId, c.postId)])
    let stage2 =
        let offsets = byContext |> Array.scan (fun offset (_, rows) -> offset + rows.Length) 0
        [| for index, (_, rows) in Array.indexed byContext do
               let start = offsets.[index]
               let probabilities, _ = softmax model.stage2 secondInputs.[start .. start + rows.Length - 1]
               yield! probabilities |]
    candidates, stage1, stage2

/// Score and decide every context of one stop.
let scoreStop (model: Model) (contexts: ContextKey array) (candidates: Candidate array) : Decision array =
    let candidates, _, stage2 = stopProbabilities model contexts candidates
    let byContext = candidates |> Array.groupBy _.contextId
    let secondMass = postMass candidates stage2
    let areaMass = Dictionary<string * string, float>()
    for index = 0 to candidates.Length - 1 do
        let key = candidates.[index].contextId, candidates.[index].areaId
        areaMass.[key] <- (match areaMass.TryGetValue key with | true, value -> value | _ -> 0.0) + stage2.[index]
    let contextKeys = contexts |> Array.map (fun c -> c.contextId, c) |> dict
    let indexed = candidates |> Array.mapi (fun index c -> c, stage2.[index])
    [| for contextId, _ in byContext do
           let rows = indexed |> Array.filter (fun (c, _) -> c.contextId = contextId)
           let post (c: Candidate) = min 1.0 secondMass.[(c.contextId, c.postId)]
           let area (c: Candidate) = areaMass.[(c.contextId, c.areaId)]
           let platform (c: Candidate) = if c.isPlatform then 1 else 0
           let best key =
               rows |> Array.sortWith (fun (left, leftP) (right, rightP) ->
                   let comparisons = [|
                       compare (key right) (key left)
                       compare (platform right) (platform left)
                       compare rightP leftP
                       String.CompareOrdinal(left.hypothesisId, right.hypothesisId) |]
                   comparisons |> Array.tryFind ((<>) 0) |> Option.defaultValue 0)
               |> Array.head
           let top, _ = best (fun c -> post c)
           let areaTop, _ = best (fun c -> area c, post c)
           let complexStop = contextKeys.[contextId].stopPostCount >= ComplexStopPosts
           let threshold = if complexStop then model.thresholds.complexStop else model.thresholds.simple
           if post top >= threshold then
               yield { contextId = contextId; resolution = Physical; hypothesisId = Some top.hypothesisId
                       postProbability = post top; areaProbability = area top }
           else
               match model.thresholds.area with
               | Some areaThreshold when area areaTop >= areaThreshold ->
                   yield { contextId = contextId; resolution = Area; hypothesisId = Some areaTop.hypothesisId
                           postProbability = post areaTop; areaProbability = area areaTop }
               | _ ->
                   yield { contextId = contextId; resolution = Abstain; hypothesisId = None
                           postProbability = post top; areaProbability = area top } |]
