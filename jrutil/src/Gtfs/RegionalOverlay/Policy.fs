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
open JrUtil.RegionalOverlay.Values
open JrUtil.RegionalOverlay.GtfsFiles
open JrUtil.RegionalOverlay.StopGroups
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
    if policy.source.stopMatch.coordinateIdentityMaximumMetres < 0.0 then
        invalidArg "--policy" "coordinate_identity_maximum_metres must not be negative"
    if policy.source.stopMatch.coordinateIdentityMinimumMarginMetres < 0.0 then
        invalidArg "--policy" "coordinate_identity_minimum_margin_metres must not be negative"
    if isNull (box policy.source.stopMatch.contextualInference) then
        invalidArg "--policy" "source.stop_match.contextual_inference is required"
    if policy.source.stopMatch.contextualInference.minimumMappedCalls < 1 then
        invalidArg "--policy" "contextual inference minimum_mapped_calls must be positive"
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
    let routeTiers = set [ "companion_assertion"; "structural_trip_evidence" ]
    if isNull policy.source.tripSetAuthority.modes then
        invalidArg "--policy" "source.trip_set_authority.modes is required"
    if isNull policy.source.tripSetAuthority.sourceNativeModes then
        invalidArg "--policy" "source.trip_set_authority.source_native_modes is required"
    let authorityModes = set [ "bus"; "tram"; "metro"; "trolleybus"; "ferry"; "other-guided" ]
    for mode in policy.source.tripSetAuthority.modes do
        if not (authorityModes.Contains(mode)) then
            invalidArg "--policy" $"Unsupported source.trip_set_authority mode: {mode}"
    for mode in policy.source.tripSetAuthority.sourceNativeModes do
        if not (authorityModes.Contains(mode)) then
            invalidArg "--policy" $"Unsupported source.trip_set_authority source-native mode: {mode}"
        if not (policy.source.tripSetAuthority.modes |> Array.contains mode) then
            invalidArg "--policy" $"Source-native mode must also be authoritative: {mode}"
    if not (isNull policy.source.tripSetAuthority.idsSystemIds) && policy.source.tripSetAuthority.idsSystemIds.Length > 0 then
        for system in policy.source.tripSetAuthority.idsSystemIds do
            requiredText "source.trip_set_authority.ids_system_ids" system |> ignore
        if policy.source.tripSetAuthority.maximumSilencedLines < 1 then
            invalidArg "--policy" "source.trip_set_authority.maximum_silenced_lines must be positive with ids_system_ids"
    let tripTiers =
        set [
            "full_signature"; "pattern_endpoints"; "pattern_first"; "pattern_nearest"
            "pattern_edit_nearest"
        ]
    if isNull policy.source.routeMatchTiers || policy.source.routeMatchTiers.Length = 0 then
        invalidArg "--policy" "source.route_match_tiers is required"
    if isNull policy.source.tripMatchTiers || policy.source.tripMatchTiers.Length = 0 then
        invalidArg "--policy" "source.trip_match_tiers is required"
    for tier in policy.source.routeMatchTiers do
        if not (routeTiers.Contains(tier)) then invalidArg "--policy" $"Unknown route matching tier: {tier}"
    for tier in policy.source.tripMatchTiers do
        if not (tripTiers.Contains(tier)) then invalidArg "--policy" $"Unknown trip matching tier: {tier}"
    if isNull policy.source.capabilities then
        invalidArg "--policy" "source.capabilities is required"
    for name in policy.source.capabilities do
        if not (capabilityNames.Contains(name)) then
            invalidArg "--policy" $"Unknown or base-only overlay capability: {name}"
    policy

/// Carries each source's scopes through the combined multi-source payload.
let idsAuthorityTable = "overlay_ids_authority.txt"
let idsAuthorityColumns = [| "source_id"; "ids_system_id"; "modes"; "excluded_cis_lines"; "window_start"; "window_end"; "maximum_silenced_lines" |]

/// The dates a source publishes: feed_info's window, else its calendar's extent.
let sourceFeedWindow payloadPath =
    match csvRows payloadPath "feed_info.txt" |> Seq.tryHead with
    | Some row when rowValue row "feed_start_date" <> "" && rowValue row "feed_end_date" <> "" ->
        Some (parseDate (rowValue row "feed_start_date"), parseDate (rowValue row "feed_end_date"))
    | _ ->
        let dates =
            seq {
                for row in csvRows payloadPath "calendar.txt" do
                    yield parseDate (rowValue row "start_date")
                    yield parseDate (rowValue row "end_date")
                for row in csvRows payloadPath "calendar_dates.txt" do
                    if rowValue row "exception_type" = "1" then yield parseDate (rowValue row "date")
            }
            |> Seq.toArray
        if dates.Length = 0 then None else Some (Array.min dates, Array.max dates)

let idsAuthorityScopes (source: SourcePolicy) payloadPath =
    let authority = source.tripSetAuthority
    if isNull authority.idsSystemIds || authority.idsSystemIds.Length = 0 then [||]
    else
        match sourceFeedWindow payloadPath with
        | None -> [||]
        | Some (start, finish) ->
            authority.idsSystemIds
            |> Array.map (fun system -> {
                sourceId = source.sourceId; idsSystemId = system; modes = authority.modes
                excludedCisLines = if isNull authority.excludedCisLines then [||] else authority.excludedCisLines
                windowStart = start; windowEnd = finish; maximumSilencedLines = authority.maximumSilencedLines })

let idsAuthorityRow (scope: IdsAuthorityScope) =
    [| scope.sourceId; scope.idsSystemId; String.concat ";" scope.modes; String.concat ";" scope.excludedCisLines
       dateString scope.windowStart; dateString scope.windowEnd; string scope.maximumSilencedLines |]

let readIdsAuthorityScopes payloadPath =
    csvRows payloadPath idsAuthorityTable
    |> Seq.map (fun row -> {
        sourceId = rowValue row "source_id"; idsSystemId = rowValue row "ids_system_id"
        modes = (rowValue row "modes").Split(';', StringSplitOptions.RemoveEmptyEntries)
        excludedCisLines = (rowValue row "excluded_cis_lines").Split(';', StringSplitOptions.RemoveEmptyEntries)
        windowStart = parseDate (rowValue row "window_start"); windowEnd = parseDate (rowValue row "window_end")
        maximumSilencedLines = int (rowValue row "maximum_silenced_lines") })
    |> Seq.toArray
