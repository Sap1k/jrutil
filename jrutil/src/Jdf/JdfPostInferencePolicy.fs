// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

module JrUtil.JdfPostInferencePolicy

open System
open System.Collections.Concurrent
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json

module PostInferencePhaseProbe =
    let private counts=ConcurrentDictionary<string,int64>(StringComparer.Ordinal)

    let record phase =
        counts.AddOrUpdate(phase,1L,fun _ value -> value+1L) |> ignore

    let reset () = counts.Clear()

    let snapshot () =
        counts |> Seq.map(fun pair -> pair.Key,pair.Value) |> Map.ofSeq

[<Literal>]
let PolicySchemaVersion = 2

[<Literal>]
let DefaultCapturedRoutedExcessHorizonMetres = 1000.0

[<Literal>]
let MaximumCapturedCorridorVariants = 3

type ConsolidationPolicy = {
    maximumDiameterMetres: float
    maximumChainageDifferenceMetres: float
    maximumHeadingDifferenceDegrees: float
    oppositeSideToleranceMetres: float
    distinguishedContextMinimumCount: int
    distinguishedContextMinimumScore: float
    distinguishedContextMinimumMargin: float
}

type GeometryWeights = {
    alignment: float
    side: float
    proximity: float
    routedExcess: float
}

type HardGatePolicy = {
    centrelineNeutralToleranceMetres: float
    maximumCorridorDistanceMetres: float
    maximumRoutedExcessMetres: float
}

type ResolutionPolicy = {
    minimumPlausibleScore: float
    minimumPhysicalScore: float
    minimumPhysicalMargin: float
    materialRoutedAdvantage: float
}

type ConsensusPolicy = {
    minimumContexts: int
    minimumWinningShare: float
    minimumPerContextScore: float
    minimumPerContextLead: float
}

type EstablishedPostPolicy = {
    minimumSupportingContexts: int
    minimumRunnerUpRatio: float
    maximumGeometryDisadvantage: float
    maximumPopularityAdjustment: float
    perContextAdjustment: float
}

type AlternativeCorridorPolicy = {
    absoluteTieMetres: float
    relativeTieFraction: float
    maximumVariants: int
}

type ModalityPolicy = {
    sourceSupportPerWeight: float
    maximumSourceSupportAdjustment: float
    explicitSupportAdjustment: float
    estimatedSupportAdjustment: float
    conflictAdjustment: float
    maximumCombinedSupportingAdjustment: float
}

type SpatialIsolationPolicy = {
    minimumSeparationMetres: float
    maximumAdjustment: float
}

type SideGroupPolicy = {
    maximumOrdinaryGroups: int
    maximumCompactnessMetres: float
    additionalGroupMinimumContexts: int
}

type AuthoredResolutionPolicy = {
    minimumPhysicalScore: float
    minimumPhysicalMargin: float
    requireUnanimousContexts: bool
}

type SameStopPairPolicy = {
    enabled: bool
    minimumIndividualScore: float
    minimumIndividualMargin: float
}

type PostInferencePolicyV2 = {
    schemaVersion: int
    policyId: string
    consolidation: ConsolidationPolicy
    geometryWeights: GeometryWeights
    hardGates: HardGatePolicy
    spatialIsolation: SpatialIsolationPolicy
    resolution: ResolutionPolicy
    consensus: ConsensusPolicy
    establishedPost: EstablishedPostPolicy
    alternativeCorridors: AlternativeCorridorPolicy
    modality: ModalityPolicy
    sideGroups: SideGroupPolicy
    authoredResolution: AuthoredResolutionPolicy
    sameStopPairs: SameStopPairPolicy
}

let conservativeRoutedV4 = {
    schemaVersion=PolicySchemaVersion
    policyId="conservative-routed-v4|tuned-safe-v3|kostany-diagnostic-best-safe"
    consolidation={
        maximumDiameterMetres=2.0
        maximumChainageDifferenceMetres=20.0
        maximumHeadingDifferenceDegrees=30.0
        oppositeSideToleranceMetres=2.0
        distinguishedContextMinimumCount=2
        distinguishedContextMinimumScore=0.55
        distinguishedContextMinimumMargin=0.15 }
    geometryWeights={ alignment=0.40; side=0.25; proximity=0.20; routedExcess=0.15 }
    hardGates={
        centrelineNeutralToleranceMetres=2.0
        maximumCorridorDistanceMetres=100.0
        maximumRoutedExcessMetres=500.0 }
    spatialIsolation={ minimumSeparationMetres=12.0; maximumAdjustment=0.05 }
    resolution={
        minimumPlausibleScore=0.90
        minimumPhysicalScore=0.98
        minimumPhysicalMargin=0.02
        materialRoutedAdvantage=0.05 }
    consensus={
        minimumContexts=1
        minimumWinningShare=0.67
        minimumPerContextScore=0.90
        minimumPerContextLead=0.02 }
    establishedPost={
        minimumSupportingContexts=3
        minimumRunnerUpRatio=2.0
        maximumGeometryDisadvantage=0.15
        maximumPopularityAdjustment=0.10
        perContextAdjustment=0.02 }
    alternativeCorridors={ absoluteTieMetres=0.0; relativeTieFraction=0.02; maximumVariants=3 }
    modality={
        sourceSupportPerWeight=0.01
        maximumSourceSupportAdjustment=0.05
        explicitSupportAdjustment=0.05
        estimatedSupportAdjustment=0.03
        conflictAdjustment = -0.05
        maximumCombinedSupportingAdjustment=0.10 }
    sideGroups={
        maximumOrdinaryGroups=2
        maximumCompactnessMetres=75.0
        additionalGroupMinimumContexts=2 }
    authoredResolution={
        minimumPhysicalScore=0.75
        minimumPhysicalMargin=0.20
        requireUnanimousContexts=true }
    sameStopPairs={
        enabled=true
        minimumIndividualScore=0.70
        minimumIndividualMargin=0.15 }
}

let private jsonOptions =
    JsonSerializerOptions(PropertyNamingPolicy=JsonNamingPolicy.SnakeCaseLower,
                          WriteIndented=true)

let private fail field message = invalidArg "policy" $"{field}: {message}"

let private finite field value =
    if not(Double.IsFinite value) then fail field "must be finite"
    value

let private nonNegative field value =
    finite field value |> ignore
    if value < 0.0 then fail field "must be non-negative"

let private positive field value =
    finite field value |> ignore
    if value <= 0.0 then fail field "must be positive"

let private unitInterval field value =
    finite field value |> ignore
    if value < 0.0 || value > 1.0 then fail field "must be in [0,1]"

let private positiveCount field value =
    if value < 1 then fail field "must be at least one"

let validatePolicy capturedHorizon (policy:PostInferencePolicyV2) =
    if policy.schemaVersion<>PolicySchemaVersion then
        fail "schema_version" $"unsupported schema {policy.schemaVersion}; expected {PolicySchemaVersion}"
    if String.IsNullOrWhiteSpace(policy.policyId) then fail "policy_id" "is required"
    positive "captured routed-excess horizon" capturedHorizon

    let c=policy.consolidation
    positive "consolidation.maximum_diameter_metres" c.maximumDiameterMetres
    nonNegative "consolidation.maximum_chainage_difference_metres" c.maximumChainageDifferenceMetres
    if c.maximumHeadingDifferenceDegrees < 0.0 || c.maximumHeadingDifferenceDegrees > 180.0
       || not(Double.IsFinite c.maximumHeadingDifferenceDegrees) then
        fail "consolidation.maximum_heading_difference_degrees" "must be in [0,180]"
    nonNegative "consolidation.opposite_side_tolerance_metres" c.oppositeSideToleranceMetres
    positiveCount "consolidation.distinguished_context_minimum_count" c.distinguishedContextMinimumCount
    unitInterval "consolidation.distinguished_context_minimum_score" c.distinguishedContextMinimumScore
    unitInterval "consolidation.distinguished_context_minimum_margin" c.distinguishedContextMinimumMargin

    let weights=policy.geometryWeights
    let weightValues=[|weights.alignment;weights.side;weights.proximity;weights.routedExcess|]
    weightValues |> Array.iteri(fun index value -> nonNegative $"geometry_weights[{index}]" value)
    let weightSum=weightValues |> Array.sum
    if abs(weightSum-1.0)>0.000001 then fail "geometry_weights" $"must sum to 1.0, got {weightSum}"

    let gates=policy.hardGates
    nonNegative "hard_gates.centreline_neutral_tolerance_metres" gates.centrelineNeutralToleranceMetres
    positive "hard_gates.maximum_corridor_distance_metres" gates.maximumCorridorDistanceMetres
    positive "hard_gates.maximum_routed_excess_metres" gates.maximumRoutedExcessMetres
    if gates.maximumRoutedExcessMetres>capturedHorizon then
        fail "hard_gates.maximum_routed_excess_metres"
             $"requests {gates.maximumRoutedExcessMetres} m but evidence captures {capturedHorizon} m"

    nonNegative "spatial_isolation.minimum_separation_metres" policy.spatialIsolation.minimumSeparationMetres
    unitInterval "spatial_isolation.maximum_adjustment" policy.spatialIsolation.maximumAdjustment

    let resolution=policy.resolution
    unitInterval "resolution.minimum_plausible_score" resolution.minimumPlausibleScore
    unitInterval "resolution.minimum_physical_score" resolution.minimumPhysicalScore
    unitInterval "resolution.minimum_physical_margin" resolution.minimumPhysicalMargin
    unitInterval "resolution.material_routed_advantage" resolution.materialRoutedAdvantage
    if resolution.minimumPlausibleScore>resolution.minimumPhysicalScore then
        fail "resolution" "minimum_plausible_score cannot exceed minimum_physical_score"

    let consensus=policy.consensus
    positiveCount "consensus.minimum_contexts" consensus.minimumContexts
    unitInterval "consensus.minimum_winning_share" consensus.minimumWinningShare
    unitInterval "consensus.minimum_per_context_score" consensus.minimumPerContextScore
    unitInterval "consensus.minimum_per_context_lead" consensus.minimumPerContextLead

    let established=policy.establishedPost
    positiveCount "established_post.minimum_supporting_contexts" established.minimumSupportingContexts
    if established.minimumRunnerUpRatio < 1.0 || not(Double.IsFinite established.minimumRunnerUpRatio) then
        fail "established_post.minimum_runner_up_ratio" "must be finite and at least one"
    unitInterval "established_post.maximum_geometry_disadvantage" established.maximumGeometryDisadvantage
    unitInterval "established_post.maximum_popularity_adjustment" established.maximumPopularityAdjustment
    nonNegative "established_post.per_context_adjustment" established.perContextAdjustment
    if established.perContextAdjustment>established.maximumPopularityAdjustment then
        fail "established_post" "per_context_adjustment cannot exceed maximum_popularity_adjustment"

    let alternatives=policy.alternativeCorridors
    nonNegative "alternative_corridors.absolute_tie_metres" alternatives.absoluteTieMetres
    unitInterval "alternative_corridors.relative_tie_fraction" alternatives.relativeTieFraction
    if alternatives.maximumVariants<1 || alternatives.maximumVariants>MaximumCapturedCorridorVariants then
        fail "alternative_corridors.maximum_variants"
             $"must be between 1 and capture ceiling {MaximumCapturedCorridorVariants}"

    let modality=policy.modality
    nonNegative "modality.source_support_per_weight" modality.sourceSupportPerWeight
    unitInterval "modality.maximum_source_support_adjustment" modality.maximumSourceSupportAdjustment
    unitInterval "modality.explicit_support_adjustment" modality.explicitSupportAdjustment
    unitInterval "modality.estimated_support_adjustment" modality.estimatedSupportAdjustment
    if modality.conflictAdjustment > 0.0 || modality.conflictAdjustment < -1.0
       || not(Double.IsFinite modality.conflictAdjustment) then
        fail "modality.conflict_adjustment" "must be in [-1,0]"
    unitInterval "modality.maximum_combined_supporting_adjustment" modality.maximumCombinedSupportingAdjustment
    if modality.explicitSupportAdjustment+modality.estimatedSupportAdjustment
       < modality.maximumCombinedSupportingAdjustment then ()

    positiveCount "side_groups.maximum_ordinary_groups" policy.sideGroups.maximumOrdinaryGroups
    positive "side_groups.maximum_compactness_metres" policy.sideGroups.maximumCompactnessMetres
    positiveCount "side_groups.additional_group_minimum_contexts" policy.sideGroups.additionalGroupMinimumContexts

    unitInterval "authored_resolution.minimum_physical_score" policy.authoredResolution.minimumPhysicalScore
    unitInterval "authored_resolution.minimum_physical_margin" policy.authoredResolution.minimumPhysicalMargin
    unitInterval "same_stop_pairs.minimum_individual_score" policy.sameStopPairs.minimumIndividualScore
    unitInterval "same_stop_pairs.minimum_individual_margin" policy.sameStopPairs.minimumIndividualMargin
    policy

let validatePolicyForEvidence capturedHorizon capturedMaximumVariants policy =
    let validated=validatePolicy capturedHorizon policy
    if policy.alternativeCorridors.maximumVariants>capturedMaximumVariants then
        fail "alternative_corridors.maximum_variants"
             $"requests {policy.alternativeCorridors.maximumVariants} but evidence captures {capturedMaximumVariants}"
    validated

let loadPolicy path =
    PostInferencePhaseProbe.record "policy-loading"
    if not(File.Exists path) then invalidArg "path" $"Post-inference policy does not exist: {path}"
    let policy=JsonSerializer.Deserialize<PostInferencePolicyV2>(File.ReadAllText(path),jsonOptions)
    if isNull(box policy) then invalidArg "path" $"Post-inference policy is empty: {path}"
    validatePolicy DefaultCapturedRoutedExcessHorizonMetres policy

[<Literal>]
let PolicyDocumentSchemaVersion = 3

/// How candidates are ranked and decided: the tuned heuristic (policy v2), or the
/// learned two-stage scorer (jrutil/scripts/post-scorer) embedded in a v3 document.
type ScorerChoice =
    | HeuristicScorer
    | LearnedScorer of JdfPostScorer.Model

type LoadedPolicy = {
    policy: PostInferencePolicyV2
    scorer: ScorerChoice
    documentSha256: string
}

/// Load a v2 policy file (heuristic scorer) or a v3 document
/// `{"schema_version":3,"policy":{…v2…},"scorer":{"kind":"heuristic"|"learned","model":{…}}}`.
/// The document hash covers the embedded model.
let loadPolicyWithScorer path =
    PostInferencePhaseProbe.record "policy-loading"
    if not(File.Exists path) then invalidArg "path" $"Post-inference policy does not exist: {path}"
    let text = File.ReadAllText(path)
    let documentSha256 =
        Encoding.UTF8.GetBytes(text) |> SHA256.HashData |> Convert.ToHexString |> _.ToLowerInvariant()
    use document = JsonDocument.Parse(text)
    let root = document.RootElement
    let version =
        match root.TryGetProperty("schema_version") with
        | true, value when value.ValueKind = JsonValueKind.Number -> value.GetInt32()
        | _ -> invalidArg "path" "Post-inference policy has no schema_version"
    let parsePolicy (element: JsonElement) =
        let policy = JsonSerializer.Deserialize<PostInferencePolicyV2>(element.GetRawText(), jsonOptions)
        if isNull(box policy) then invalidArg "path" $"Post-inference policy is empty: {path}"
        validatePolicy DefaultCapturedRoutedExcessHorizonMetres policy
    if version = PolicySchemaVersion then
        { policy = parsePolicy root; scorer = HeuristicScorer; documentSha256 = documentSha256 }
    elif version = PolicyDocumentSchemaVersion then
        let policy =
            match root.TryGetProperty("policy") with
            | true, value when value.ValueKind = JsonValueKind.Object -> parsePolicy value
            | _ -> invalidArg "path" "Policy document v3 requires a policy object"
        let scorer =
            match root.TryGetProperty("scorer") with
            | true, value when value.ValueKind = JsonValueKind.Object ->
                match value.TryGetProperty("kind") with
                | true, kind when kind.GetString() = "heuristic" -> HeuristicScorer
                | true, kind when kind.GetString() = "learned" ->
                    match value.TryGetProperty("model") with
                    | true, model when model.ValueKind = JsonValueKind.Object ->
                        LearnedScorer(JdfPostScorer.parseModel model)
                    | _ -> invalidArg "path" "A learned scorer requires a model object"
                | _ -> invalidArg "path" "Scorer kind must be heuristic or learned"
            | _ -> invalidArg "path" "Policy document v3 requires a scorer object"
        { policy = policy; scorer = scorer; documentSha256 = documentSha256 }
    else
        invalidArg "path" $"Unsupported policy schema {version}; expected {PolicySchemaVersion} or {PolicyDocumentSchemaVersion}"

let writePolicy path (policy:PostInferencePolicyV2) =
    validatePolicy DefaultCapturedRoutedExcessHorizonMetres policy |> ignore
    File.WriteAllText(path,JsonSerializer.Serialize(policy,jsonOptions)+"\n")

let policySha256 policy =
    JsonSerializer.Serialize(policy,jsonOptions) |> Encoding.UTF8.GetBytes
    |> SHA256.HashData |> Convert.ToHexString |> _.ToLowerInvariant()
