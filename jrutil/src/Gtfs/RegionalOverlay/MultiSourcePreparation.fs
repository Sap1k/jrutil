// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.MultiSourcePreparation

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Text
open System.Text.Json
open System.Text.RegularExpressions

open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay.Policy

type Prepared = {
    policyPath: string
    binding: SourceBinding
    sourceIds: string array
    descriptors: IReadOnlyDictionary<string, SourceDescriptor>
}

let private resolveRelative (owner: string) (value: string) =
    if Path.IsPathRooted(value) then Path.GetFullPath(value)
    else Path.GetFullPath(Path.Combine(Path.GetDirectoryName(owner), value))

let loadPolicy path =
    let fullPath = Path.GetFullPath(path)
    let policy = JsonSerializer.Deserialize<OverlayAllPolicy>(File.ReadAllText(fullPath), jsonOptions)
    if isNull (box policy) then invalidArg "--policy" "Multi-source overlay policy is empty"
    if policy.schemaVersion <> OverlayAllPolicySchemaVersion then
        invalidArg "--policy" $"Unsupported multi-source overlay policy schema {policy.schemaVersion}"
    if policy.conflictPolicy <> "equal_priority_quarantine" then
        invalidArg "--policy" "conflict_policy must be equal_priority_quarantine"
    if policy.publicationEnabled && policy.calibration then
        invalidArg "--policy" "A calibration policy cannot enable publication"
    if isNull policy.sources || policy.sources.Length < 2 then
        invalidArg "--policy" "Multi-source overlay policy requires at least two sources"
    let sourceIds = policy.sources |> Array.map (fun source -> requiredText "sources.source_id" source.sourceId)
    if sourceIds |> Array.distinct |> Array.length <> sourceIds.Length then
        invalidArg "--policy" "Multi-source overlay source IDs must be unique"
    for source in policy.sources do
        if source.adapter <> "pid-v1" && source.adapter <> "ids-jmk-v1" then
            invalidArg "--policy" $"Unsupported regional overlay adapter: {source.adapter}"
        let sourcePolicy = Policy.loadPolicy (resolveRelative fullPath source.policy)
        if sourcePolicy.source.sourceId <> source.sourceId then
            invalidArg "--policy" $"Profile source {sourcePolicy.source.sourceId} does not match {source.sourceId}"
    fullPath, policy

let private namespaced sourceId value = sourceId + ":" + value

let private rewriteId sourceId column (row: CsvRow) =
    let value = rowValue row column
    if not (String.IsNullOrWhiteSpace(value)) then
        row.["overlay_original_" + column] <- value
        row.[column] <- namespaced sourceId value

let private normalizeRow adapter sourceId table (row: CsvRow) =
    let row = cloneRow row
    row.["overlay_source_id"] <- sourceId
    match table with
    | "agency.txt" -> rewriteId sourceId "agency_id" row
    | "routes.txt" ->
        rewriteId sourceId "route_id" row
        rewriteId sourceId "agency_id" row
    | "trips.txt" ->
        rewriteId sourceId "route_id" row
        rewriteId sourceId "service_id" row
        rewriteId sourceId "trip_id" row
        rewriteId sourceId "block_id" row
        rewriteId sourceId "shape_id" row
        let revision =
            if adapter = "pid-v1" then
                let original = rowValue row "overlay_original_trip_id"
                let matched = Regex.Match(original, "_(?<revision>\\d{6})(?:_|$)", RegexOptions.CultureInvariant)
                if matched.Success then matched.Groups.["revision"].Value else "700101"
            else "700101"
        row.["overlay_revision"] <- revision
    | "stops.txt" ->
        let originalStop = rowValue row "stop_id"
        let originalParent = rowValue row "parent_station"
        row.["overlay_group_id"] <-
            if adapter = "pid-v1" then
                match rowValue row "asw_node_id" with
                | value when not (String.IsNullOrWhiteSpace(value)) -> namespaced sourceId ("group:" + value)
                | _ when not (String.IsNullOrWhiteSpace(originalParent)) -> namespaced sourceId ("group:" + originalParent)
                | _ -> namespaced sourceId ("group:" + originalStop)
            elif not (String.IsNullOrWhiteSpace(originalParent)) then namespaced sourceId ("group:" + originalParent)
            else namespaced sourceId ("group:" + originalStop)
        row.["overlay_post_id"] <-
            if rowValue row "location_type" = "1" then ""
            elif adapter = "pid-v1" && not (String.IsNullOrWhiteSpace(rowValue row "asw_stop_id")) then rowValue row "asw_stop_id"
            else originalStop
        rewriteId sourceId "stop_id" row
        rewriteId sourceId "parent_station" row
        rewriteId sourceId "level_id" row
    | "stop_times.txt" ->
        rewriteId sourceId "trip_id" row
        rewriteId sourceId "stop_id" row
    | "calendar.txt" | "calendar_dates.txt" -> rewriteId sourceId "service_id" row
    | "shapes.txt" -> rewriteId sourceId "shape_id" row
    | "transfers.txt" ->
        for column in [| "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id" |] do
            rewriteId sourceId column row
    | _ -> ()
    row

let private columnsFor bindings table =
    seq {
        for _, binding, _, _ in bindings do
            yield! columnsOf binding.payloadPath table
        yield "overlay_source_id"
        match table with
        | "agency.txt" -> yield "overlay_original_agency_id"
        | "routes.txt" -> yield "overlay_original_route_id"; yield "overlay_original_agency_id"
        | "trips.txt" ->
            yield! [ "overlay_original_route_id"; "overlay_original_service_id"; "overlay_original_trip_id"
                     "overlay_original_block_id"; "overlay_original_shape_id"; "overlay_revision"
                     "overlay_source_native_allowed"; "overlay_trip_set_authority_allowed" ]
        | "stops.txt" ->
            yield! [ "overlay_original_stop_id"; "overlay_original_parent_station"; "overlay_original_level_id"
                     "overlay_group_id"; "overlay_post_id" ]
        | "stop_times.txt" -> yield "overlay_original_trip_id"; yield "overlay_original_stop_id"
        | "calendar.txt" | "calendar_dates.txt" -> yield "overlay_original_service_id"
        | "shapes.txt" -> yield "overlay_original_shape_id"
        | "transfers.txt" ->
            for column in [| "from_stop_id"; "to_stop_id"; "from_route_id"; "to_route_id"; "from_trip_id"; "to_trip_id" |] do
                yield "overlay_original_" + column
        | _ -> ()
    }
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> Seq.distinct
    |> Seq.toArray

let private writeCombinedTables root bindings =
    for table in [| "agency.txt"; "routes.txt"; "trips.txt"; "stops.txt"; "stop_times.txt"; "calendar.txt"; "calendar_dates.txt"; "shapes.txt"; "transfers.txt" |] do
        let columns = columnsFor bindings table
        let rows = seq {
            for adapter, binding, _, sourcePolicy in bindings do
                let routeModes =
                    if table = "trips.txt" then
                        csvRows binding.payloadPath "routes.txt"
                        |> Seq.map (fun route -> rowValue route "route_id", modeClass (rowValue route "route_type"))
                        |> dict
                    else dict (Seq.empty<string * string>)
                for row in csvRows binding.payloadPath table do
                    if table <> "routes.txt" || modeClass (rowValue row "route_type") <> "heavy-rail" then
                        let normalized = normalizeRow adapter binding.sourceId table row
                        if table = "trips.txt" then
                            let mode = routeModes.[rowValue row "route_id"]
                            normalized.["overlay_source_native_allowed"] <-
                                if sourcePolicy.source.tripSetAuthority.sourceNativeModes |> Array.contains mode then "1" else "0"
                            normalized.["overlay_trip_set_authority_allowed"] <-
                                if sourcePolicy.source.tripSetAuthority.modes |> Array.contains mode then "1" else "0"
                        yield normalized
        }
        if table <> "shapes.txt" || bindings |> Array.exists (fun (_, binding, _, _) -> (columnsOf binding.payloadPath table).Length > 0) then
            writeRows (Path.Combine(root, table)) columns rows

let private writeRouteJoin root bindings =
    let rows = seq {
        for adapter, binding, _, _ in bindings do
            if adapter = "pid-v1" then
                let lookup =
                    csvRows binding.payloadPath "route_sub_agencies.txt"
                    |> Seq.groupBy (fun row -> rowValue row "route_id" + "\u001f" + rowValue row "sub_agency_id")
                    |> Seq.map (fun (key, values) -> key, values |> Seq.map (fun row -> rowValue row "route_licence_number") |> Seq.filter (String.IsNullOrWhiteSpace >> not) |> Seq.distinct |> Seq.toArray)
                    |> dict
                for trip in csvRows binding.payloadPath "trips.txt" do
                    let key = rowValue trip "route_id" + "\u001f" + rowValue trip "sub_agency_id"
                    match lookup.TryGetValue(key) with
                    | true, values when values.Length = 1 -> yield [| namespaced binding.sourceId (rowValue trip "trip_id"); values.[0] |]
                    | _ -> ()
    }
    writeValues (Path.Combine(root, "overlay_route_join.txt")) [| "trip_id"; "cis_line_id" |] rows

let private writeIdsJmkOperational root bindings =
    let rows = ResizeArray<string array>()
    for adapter, binding, _, _ in bindings do
        if adapter = "ids-jmk-v1" then
            let knownTrips = csvRows binding.payloadPath "trips.txt" |> Seq.map (fun row -> rowValue row "trip_id") |> Set.ofSeq
            let seenTrips = HashSet<string>(StringComparer.Ordinal)
            let pattern = Regex("^Linka/CVlaku = trip_id: (?<line>[^/]+)/(?<course>.+) = (?<trip>[^=\\s]+)$", RegexOptions.CultureInvariant)
            let mutable lineNumber = 0
            for line in textLines binding.payloadPath "api.txt" do
                lineNumber <- lineNumber + 1
                if not (String.IsNullOrWhiteSpace(line)) then
                    let matched = pattern.Match(line.Trim())
                    if not matched.Success then invalidOp $"IDS JMK api.txt line {lineNumber} is malformed"
                    let trip = matched.Groups.["trip"].Value
                    if not (knownTrips.Contains(trip)) then invalidOp $"IDS JMK api.txt references missing trip {trip}"
                    if not (seenTrips.Add(trip)) then invalidOp $"IDS JMK api.txt maps trip {trip} more than once"
                    rows.Add [| binding.sourceId; matched.Groups.["line"].Value; matched.Groups.["course"].Value; namespaced binding.sourceId trip; trip |]
            if seenTrips.Count <> knownTrips.Count then
                invalidOp $"IDS JMK api.txt maps {seenTrips.Count} of {knownTrips.Count} trips"
    writeValues (Path.Combine(root, "operational_trip_candidates.txt"))
        [| "source_id"; "operational_line_id"; "operational_trip_id"; "trip_id"; "original_trip_id" |] rows

let private writeCombinedOverrides destination (preparedBindings: (string * SourceBinding * string * OverlayPolicy) array) : OverridePolicy =
    let columns = [| "source_namespace"; "source_id"; "target_namespace"; "target_id"; "valid_from"; "valid_to"; "review_note" |]
    let write kind configured =
        let output = Path.Combine(destination, "combined-" + kind + "-overrides.csv")
        let rows = seq {
            for _, binding, policyPath, sourcePolicy in preparedBindings do
                let configuredPath = configured sourcePolicy.source.overrides
                if not (String.IsNullOrWhiteSpace(configuredPath)) then
                    let path = resolveRelative policyPath configuredPath
                    for row in csvRows (Path.GetDirectoryName(path)) (Path.GetFileName(path)) do
                        let copy = cloneRow row
                        copy.["source_id"] <- namespaced binding.sourceId (rowValue row "source_id")
                        yield copy
        }
        writeRows output columns rows
        Path.GetFileName(output)
    {
        routes = write "route" (fun value -> value.routes)
        trips = write "trip" (fun value -> value.trips)
        stops = write "stop" (fun value -> value.stops)
    }

let prepare scratchRoot combinedPolicyPath baseBundle (bindings: SourceBinding array) =
    let combinedPolicyPath, combinedPolicy = loadPolicy combinedPolicyPath
    let byId = bindings |> Array.map (fun binding -> binding.sourceId, binding) |> dict
    if byId.Count <> bindings.Length then invalidArg "--source" "Duplicate source binding"
    let configuredIds = combinedPolicy.sources |> Array.map (fun source -> source.sourceId) |> Set.ofArray
    if (bindings |> Array.map (fun binding -> binding.sourceId) |> Set.ofArray) <> configuredIds then
        invalidArg "--source" "Bindings must exactly match the multi-source policy"
    let normalized = Path.Combine(scratchRoot, "combined-source")
    Directory.CreateDirectory(normalized) |> ignore
    let descriptors = Dictionary<string, SourceDescriptor>(StringComparer.Ordinal)
    let baseRetrievedAt = descriptorRetrievedAtFromBase baseBundle
    let preparedBindings =
        combinedPolicy.sources
        |> Array.sortBy (fun source -> source.sourceId)
        |> Array.map (fun source ->
            let binding = byId.[source.sourceId]
            let descriptor = readDescriptor binding.descriptorPath
            let actualHash = if File.Exists(binding.payloadPath) then sha256File binding.payloadPath else sha256Tree binding.payloadPath
            if actualHash <> descriptor.payloadSha256 then invalidOp $"Source checksum mismatch for {source.sourceId}"
            let sourcePolicyPath = resolveRelative combinedPolicyPath source.policy
            let sourcePolicy = Policy.loadPolicy sourcePolicyPath
            let snapshotSkew = abs ((descriptor.retrievedAt - baseRetrievedAt).TotalDays)
            if snapshotSkew > float sourcePolicy.source.maximumSnapshotSkewDays then
                invalidOp $"Source/base snapshot skew for {source.sourceId} is {snapshotSkew:F2} days and exceeds policy maximum {sourcePolicy.source.maximumSnapshotSkewDays}"
            descriptors.[source.sourceId] <- descriptor
            source.adapter, binding, sourcePolicyPath, sourcePolicy)
    writeCombinedTables normalized preparedBindings
    writeRouteJoin normalized preparedBindings
    writeIdsJmkOperational normalized preparedBindings

    let embedded = Path.Combine(normalized, "_overlay")
    let embeddedDescriptors = Path.Combine(embedded, "descriptors")
    let embeddedPolicies = Path.Combine(embedded, "policies")
    Directory.CreateDirectory(embeddedDescriptors) |> ignore
    Directory.CreateDirectory(embeddedPolicies) |> ignore
    File.Copy(combinedPolicyPath, Path.Combine(embeddedPolicies, "overlay-all-policy.json"), true)
    let metadataSources = ResizeArray<obj>()
    for source in combinedPolicy.sources |> Array.sortBy (fun value -> value.sourceId) do
        let binding = byId.[source.sourceId]
        let descriptorName = source.sourceId + "-descriptor.json"
        File.Copy(binding.descriptorPath, Path.Combine(embeddedDescriptors, descriptorName), true)
        let profilePath = resolveRelative combinedPolicyPath source.policy
        File.Copy(profilePath, Path.Combine(embeddedPolicies, source.sourceId + "-policy.json"), true)
        let descriptor = descriptors.[source.sourceId]
        let item = Dictionary<string, obj>()
        item.["source_id"] <- box source.sourceId
        item.["payload_sha256"] <- box descriptor.payloadSha256
        item.["descriptor_sha256"] <- box (sha256File binding.descriptorPath)
        item.["retrieved_at"] <- box (descriptor.retrievedAt.ToString("O", CultureInfo.InvariantCulture))
        item.["descriptor_file"] <- box descriptorName
        item.["adapter"] <- box source.adapter
        metadataSources.Add(item)
    let metadata = Dictionary<string, obj>()
    metadata.["schema_version"] <- box 1
    metadata.["policy_sha256"] <- box (sha256File combinedPolicyPath)
    metadata.["sources"] <- box metadataSources
    File.WriteAllText(Path.Combine(normalized, "overlay_sources.json"), JsonSerializer.Serialize(metadata, jsonOptions), new UTF8Encoding(false))

    let policyDirectory = Path.Combine(scratchRoot, "combined-policy")
    Directory.CreateDirectory(policyDirectory) |> ignore
    let combinedOverrides = writeCombinedOverrides policyDirectory preparedBindings
    let template = preparedBindings.[0] |> fun (_, _, _, policy) -> policy
    let sourcePolicies = preparedBindings |> Array.map (fun (_, _, _, policy) -> policy.source)
    let capabilities = Dictionary<string, CapabilityPolicy>(StringComparer.Ordinal)
    for capabilityName in capabilityNames |> Set.toArray |> Array.sort do
        let enabledClaims =
            sourcePolicies
            |> Array.choose (fun policy ->
                match policy.capabilities.TryGetValue(capabilityName) with
                | true, value when value.mode <> "disabled" -> Some value
                | _ -> None)
            |> Array.distinct
        if enabledClaims.Length > 1 then
            invalidArg "--policy" $"Sources claim capability {capabilityName} with unequal modes or priorities"
        capabilities.[capabilityName] <-
            if enabledClaims.Length = 1 then enabledClaims.[0]
            else { mode = "disabled"; priority = 0 }
    let capabilityTiers = Dictionary<string, string>(StringComparer.Ordinal)
    for policy in sourcePolicies do
        for KeyValue(name, tier) in policy.tripMatch.minimumCapabilityTier do
            match capabilityTiers.TryGetValue(name) with
            | true, existing when existing <> tier -> invalidArg "--policy" $"Sources configure unequal minimum matching tiers for {name}"
            | _ -> capabilityTiers.[name] <- tier
    let canonicalRouteTiers = [| "companion_assertion"; "reviewed_override"; "structural_trip_evidence" |]
    let configuredRouteTiers = sourcePolicies |> Array.collect (fun policy -> policy.routeMatchTiers) |> Set.ofArray
    let canonicalTripTiers = [| "full_signature"; "pattern_endpoints"; "pattern_first"; "pattern_nearest"; "pattern_edit_nearest"; "reviewed_override" |]
    let configuredTripTiers = sourcePolicies |> Array.collect (fun policy -> policy.tripMatchTiers) |> Set.ofArray
    let generatedPolicy = {
        schemaVersion = OverlayPolicySchemaVersion
        calibration = combinedPolicy.calibration
        publicationEnabled = combinedPolicy.publicationEnabled
        minimumCoverage = combinedPolicy.minimumCoverage
        source = {
            template.source with
                sourceId = "regional-all"
                excludedRouteTypes = sourcePolicies |> Array.collect (fun policy -> policy.excludedRouteTypes) |> Array.distinct |> Array.sort
                maximumSnapshotSkewDays = 3660
                routeJoin = { table = "overlay_route_join.txt"; sourceKeys = [| "trip_id" |]; lookupKeys = [| "trip_id" |]; valueColumn = "cis_line_id"; targetNamespace = "cis_line_id" }
                stopMatch = {
                    template.source.stopMatch with
                        groupColumn = "overlay_group_id"
                        postColumn = "overlay_post_id"
                        coordinateIdentityMaximumMetres = sourcePolicies |> Array.map (fun policy -> policy.stopMatch.coordinateIdentityMaximumMetres) |> Array.max
                        coordinateIdentityMinimumMarginMetres = sourcePolicies |> Array.map (fun policy -> policy.stopMatch.coordinateIdentityMinimumMarginMetres) |> Array.max
                }
                tripMatch = { template.source.tripMatch with minimumCapabilityTier = capabilityTiers; sourceRevision = { column = "overlay_revision"; regex = "(?<revision>\\d{6})"; dateFormat = "yyMMdd" } }
                tripSetAuthority = {
                    template.source.tripSetAuthority with
                        modes = sourcePolicies |> Array.collect (fun policy -> policy.tripSetAuthority.modes) |> Array.distinct |> Array.sort
                        sourceNativeModes = sourcePolicies |> Array.collect (fun policy -> policy.tripSetAuthority.sourceNativeModes) |> Array.distinct |> Array.sort
                }
                routeMatchTiers = canonicalRouteTiers |> Array.filter configuredRouteTiers.Contains
                tripMatchTiers = canonicalTripTiers |> Array.filter configuredTripTiers.Contains
                capabilities = capabilities
                overrides = combinedOverrides
        }
    }
    let generatedPolicyPath = Path.Combine(policyDirectory, "overlay-policy.json")
    File.WriteAllText(generatedPolicyPath, JsonSerializer.Serialize(generatedPolicy, jsonOptions), new UTF8Encoding(false))
    let payloadHash = sha256Tree normalized
    let retrievedAt = descriptors.Values |> Seq.map (fun value -> value.retrievedAt) |> Seq.max
    let descriptorPath = Path.Combine(scratchRoot, "combined-descriptor.json")
    let descriptorJson = Dictionary<string, obj>()
    descriptorJson.["retrieved_at"] <- box (retrievedAt.ToString("O", CultureInfo.InvariantCulture))
    descriptorJson.["payload_sha256"] <- box payloadHash
    File.WriteAllText(descriptorPath, JsonSerializer.Serialize(descriptorJson, jsonOptions), new UTF8Encoding(false))
    {
        policyPath = generatedPolicyPath
        binding = { sourceId = "regional-all"; payloadPath = normalized; descriptorPath = descriptorPath }
        sourceIds = descriptors.Keys |> Seq.sort |> Seq.toArray
        descriptors = descriptors
    }
