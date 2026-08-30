// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.Policy

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Text.Json
open NodaTime

open JrUtil.GtfsModel

open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Support

let loadPolicy path =
    let policy =
        JsonSerializer.Deserialize<OverlayPolicy>(File.ReadAllText(path), jsonOptions)
    if isNull (box policy) then invalidArg "--policy" "Overlay policy is empty"
    if policy.schemaVersion <> OverlayPolicySchemaVersion then
        invalidArg "--policy"
            $"Unsupported overlay policy schema {policy.schemaVersion}"
    if isNull (box policy.source) then invalidArg "--policy" "source is required"
    requiredText "source.source_id" policy.source.sourceId |> ignore
    if isNull policy.source.excludedRouteTypes then
        invalidArg "--policy" "source.excluded_route_types is required"
    if policy.source.maximumSnapshotSkewDays < 0 then
        invalidArg "--policy" "maximum_snapshot_skew_days must not be negative"
    if not (isNull (box policy.source.routeJoin)) then
        let join = policy.source.routeJoin
        if join.targetNamespace <> "cis_line_id" then
            invalidArg "--policy" "source.route_join.target_namespace must be cis_line_id"
        requiredText "source.route_join.table" join.table |> ignore
        requiredText "source.route_join.value_column" join.valueColumn |> ignore
        if isNull join.sourceKeys || isNull join.lookupKeys || join.sourceKeys.Length = 0
           || join.sourceKeys.Length <> join.lookupKeys.Length then
            invalidArg "--policy" "Route join keys must be non-empty arrays of equal length"
        for key in Array.append join.sourceKeys join.lookupKeys do
            requiredText "source.route_join key" key |> ignore
    if isNull (box policy.source.stopMatch) then
        invalidArg "--policy" "source.stop_match is required"
    if policy.source.stopMatch.maximumDistanceMetres <= 0.0 then
        invalidArg "--policy" "maximum_distance_metres must be positive"
    if isNull (box policy.source.stopMatch.contextualInference) then
        invalidArg "--policy" "source.stop_match.contextual_inference is required"
    let contextual = policy.source.stopMatch.contextualInference
    if contextual.maximumUnresolvedGroupsPerTrip <> 1 then
        invalidArg "--policy" "contextual inference currently requires maximum_unresolved_groups_per_trip=1"
    if contextual.minimumMappedCalls < 1 then
        invalidArg "--policy" "contextual inference minimum_mapped_calls must be positive"
    if contextual.conflictPolicy <> "quarantine" then
        invalidArg "--policy" "contextual inference conflict_policy must be 'quarantine'"
    if isNull (box policy.source.tripMatch) then invalidArg "--policy" "source.trip_match is required"
    if isNull (box policy.source.tripSetAuthority) then invalidArg "--policy" "source.trip_set_authority is required"
    if isNull (box policy.source.tripMatch.patternEdit) then invalidArg "--policy" "trip_match.pattern_edit is required"
    if not (isNull (box policy.source.tripMatch.sourceRevision)) then
        requiredText "trip_match.source_revision.column" policy.source.tripMatch.sourceRevision.column |> ignore
        requiredText "trip_match.source_revision.regex" policy.source.tripMatch.sourceRevision.regex |> ignore
        requiredText "trip_match.source_revision.date_format" policy.source.tripMatch.sourceRevision.dateFormat |> ignore
        let revisionRegex = Regex(policy.source.tripMatch.sourceRevision.regex, RegexOptions.CultureInvariant)
        if revisionRegex.GetGroupNames() |> Array.contains "revision" |> not then
            invalidArg "--policy" "trip_match.source_revision.regex must contain a named 'revision' capture"
    if policy.source.tripMatch.patternEdit.maximumEdits < 0 then
        invalidArg "--policy" "trip_match.pattern_edit.maximum_edits must not be negative"
    if policy.source.tripMatch.patternEdit.minimumAgreement < 0.0
       || policy.source.tripMatch.patternEdit.minimumAgreement > 1.0 then
        invalidArg "--policy" "trip_match.pattern_edit.minimum_agreement must be between zero and one"
    if isNull policy.source.neverInherit
       || not (policy.source.neverInherit |> Array.contains "pathways.txt")
       || not (policy.source.neverInherit |> Array.contains "levels.txt") then
        invalidArg "--policy" "never_inherit must include pathways.txt and levels.txt in overlay bundle v1"
    let routeTiers = set [ "companion_assertion"; "reviewed_override"; "structural_trip_evidence" ]
    if isNull policy.source.tripSetAuthority.modes then
        invalidArg "--policy" "source.trip_set_authority.modes is required"
    if isNull policy.source.tripSetAuthority.sourceNativeModes then
        invalidArg "--policy" "source.trip_set_authority.source_native_modes is required"
    if policy.source.tripSetAuthority.routeMatchTier <> "companion_assertion" then
        invalidArg "--policy" "source.trip_set_authority.route_match_tier must be 'companion_assertion'"
    let authorityModes = set [ "bus"; "tram"; "metro"; "trolleybus"; "ferry"; "other-guided" ]
    for mode in policy.source.tripSetAuthority.modes do
        if not (authorityModes.Contains(mode)) then
            invalidArg "--policy" $"Unsupported source.trip_set_authority mode: {mode}"
    for mode in policy.source.tripSetAuthority.sourceNativeModes do
        if not (authorityModes.Contains(mode)) then
            invalidArg "--policy" $"Unsupported source.trip_set_authority source-native mode: {mode}"
        if not (policy.source.tripSetAuthority.modes |> Array.contains mode) then
            invalidArg "--policy" $"Source-native mode must also be authoritative: {mode}"
    let tripTiers =
        set [
            "full_signature"; "pattern_endpoints"; "pattern_first"; "pattern_nearest"
            "pattern_edit_nearest"; "reviewed_override"
        ]
    if isNull policy.source.routeMatchTiers || policy.source.routeMatchTiers.Length = 0 then
        invalidArg "--policy" "source.route_match_tiers is required"
    if isNull policy.source.tripMatchTiers || policy.source.tripMatchTiers.Length = 0 then
        invalidArg "--policy" "source.trip_match_tiers is required"
    for tier in policy.source.routeMatchTiers do
        if not (routeTiers.Contains(tier)) then invalidArg "--policy" $"Unknown route matching tier: {tier}"
    for tier in policy.source.tripMatchTiers do
        if not (tripTiers.Contains(tier)) then invalidArg "--policy" $"Unknown trip matching tier: {tier}"
    if policy.publicationEnabled && policy.calibration then
        invalidArg "--policy" "A calibration policy cannot enable publication"
    if policy.publicationEnabled && (isNull policy.minimumCoverage || policy.minimumCoverage.Count = 0) then
        invalidArg "--policy" "Publication requires reviewed minimum_coverage floors"
    if isNull policy.source.capabilities then
        invalidArg "--policy" "source.capabilities is required"
    for KeyValue(name, capability) in policy.source.capabilities do
        if not (capabilityNames.Contains(name)) then
            invalidArg "--policy" $"Unknown overlay capability: {name}"
        if not ((set ["disabled"; "fill_missing"; "preferred"; "authoritative"; "additive"]).Contains(capability.mode)) then
            invalidArg "--policy" $"Invalid mode for capability {name}: {capability.mode}"
    for forbidden in ["calendars"; "agencies"; "stop_names"; "trip_headsigns"; "trip_short_names"] do
        match policy.source.capabilities.TryGetValue(forbidden) with
        | true, value when value.mode <> "disabled" ->
            invalidArg "--policy" $"Capability {forbidden} must be disabled in overlay bundle v1"
        | _ -> ()
    policy
