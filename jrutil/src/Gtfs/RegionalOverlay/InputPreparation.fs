// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.InputPreparation

open System
open System.Collections.Generic
open System.IO
open NodaTime

open JrUtil.GtfsModel

open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Runtime
open JrUtil.RegionalOverlay.Support
open JrUtil.RegionalOverlay.Policy
open JrUtil.RegionalOverlay

type Input = {
    scratch: Scratch.Storage
    auditDate: LocalDate option
    policyPath: string
    gvdYear: int
    binding: SourceBinding
    baseBundle: string
    outputBundle: string
}

type Result = {
    shapeRows: seq<string array>
    scratch: Scratch.Storage
    policyPath: string
    baseBundle: string
    outputBundle: string
    binding: SourceBinding
    policy: OverlayPolicy
    normalizeMatchingTime: string -> string
    descriptor: SourceDescriptor
    actualSourceHash: string
    window: DateWindow
    snapshotDate: LocalDate
    auditDate: LocalDate
    baseGtfs: string
    baseExtensions: string
    diagnostics: DiagnosticLog
    stopOverrides: OverrideBinding array
    routeOverrides: OverrideBinding array
    tripOverrides: OverrideBinding array
    baseStopRows: CsvRow array
    baseRouteRows: CsvRow array
    baseTripValues: Trip array
    baseCzRouteRows: CsvRow array
    baseDates: Dictionary<string, DateSet.Dates>
    baseRoutes: IDictionary<string, CsvRow>
    baseTrips: IDictionary<string, Trip>
    baseCisByRoute: IDictionary<string, string>
    baseRoutesByCis: IDictionary<string, string array>
    basePlaceByStop: IDictionary<string, string>
    baseStopGroupById: IDictionary<string, StopGroup>
    approximateStopName: string -> bool
    stopGroupDistance: StopGroup -> StopGroup -> float option
    stableStopNameKey: string -> string
    baseStopGroupsByName: IDictionary<string, StopGroup array>
    coordinateCell: decimal -> decimal -> struct (int * int)
    coordinateCellRadius: int
    baseStopSpatial: Dictionary<struct (int * int), ResizeArray<StopGroup>>
}

/// Validate pinned inputs and build per-source working indexes over national data.
let prepare ({
    scratch = scratch
    auditDate = auditDate
    policyPath = policyPath
    gvdYear = gvdYear
    binding = binding
    baseBundle = baseBundle
    outputBundle = outputBundle
}: Input) : Result =
    let policyPath = Path.GetFullPath(policyPath)
    let baseBundle = Path.GetFullPath(baseBundle)
    let outputBundle = Path.GetFullPath(outputBundle)
    let binding = {
        binding with
            payloadPath = Path.GetFullPath(binding.payloadPath)
            descriptorPath = Path.GetFullPath(binding.descriptorPath)
    }
    validateInputs policyPath baseBundle outputBundle gvdYear binding
    let policy = loadPolicy policyPath
    let normalizeMatchingTime = matchingTimeNormalizer policy.source.tripMatch
    if binding.sourceId <> policy.source.sourceId then
        invalidArg "--source" $"Source binding {binding.sourceId} does not match policy source {policy.source.sourceId}"
    let descriptor = readDescriptor binding.descriptorPath
    let actualSourceHash =
        if File.Exists(binding.payloadPath) then sha256File binding.payloadPath
        else sha256Tree binding.payloadPath
    if actualSourceHash <> descriptor.payloadSha256 then
        invalidOp $"Source checksum mismatch: descriptor={descriptor.payloadSha256}, actual={actualSourceHash}"
    let baseRetrievedAt = descriptorRetrievedAtFromBase baseBundle
    let snapshotSkew = abs ((descriptor.retrievedAt - baseRetrievedAt).TotalDays)
    if snapshotSkew > float policy.source.maximumSnapshotSkewDays then
        invalidOp $"Source/base snapshot skew {snapshotSkew:F2} days exceeds policy maximum {policy.source.maximumSnapshotSkewDays}"

    let window = gvdWindow gvdYear
    let snapshotDate = Instant.FromDateTimeOffset(baseRetrievedAt).InZone(DateTimeZoneProviders.Tzdb.["Europe/Prague"]).Date
    let auditDate = auditDate |> Option.defaultValue snapshotDate
    if not (window.index.ContainsKey(auditDate)) then invalidArg "--audit-date" "Audit date is outside the GVD window"
    let baseGtfs = Path.Combine(baseBundle, "gtfs-intermediate")
    let baseExtensions = Path.Combine(baseBundle, "extensions")
    let diagnostics = DiagnosticLog(scratch)
    logProgress "validate-inputs" 1L (Some 1L)
    let stopOverrides = loadOverrides policyPath policy.source.overrides.stops
    let routeOverrides = loadOverrides policyPath policy.source.overrides.routes
    let tripOverrides = loadOverrides policyPath policy.source.overrides.trips

    logProgress "spool-source-shapes" 0L None
    let shapeRows = if enabled policy "shapes" then Shapes.spool scratch binding.payloadPath else Seq.empty
    logProgress "spool-source-shapes" 1L None

    let nationalBase = BasePreparation.prepare window baseGtfs baseExtensions
    // Slicing releases this source-local index; the prepared national base remains reusable.
    let baseDates = Dictionary<string, DateSet.Dates>(nationalBase.baseDates, StringComparer.Ordinal)
    let approximateStopName (value: string) = value.EndsWith(" [?]", StringComparison.Ordinal)
    let stopGroupDistance (left: StopGroup) (right: StopGroup) =
        seq {
            for leftLat, leftLon in stopGroupPoints left do
                for rightLat, rightLon in stopGroupPoints right do
                    yield haversineMetres (float leftLat) (float leftLon) (float rightLat) (float rightLon)
        } |> Seq.sort |> Seq.tryHead
    let stableStopNameKey value = normalizeName value |> String.concat "\u001f"
    let baseStopGroupsByName =
        nationalBase.baseStopGroups
        |> Array.groupBy (fun group -> stableStopNameKey group.name)
        |> dict
    let coordinateCell (lat: decimal) (lon: decimal) =
        struct (int (Math.Floor(float lat / 0.002)), int (Math.Floor(float lon / 0.002)))
    // 0.002 degrees is roughly 140 m longitudinally in Czechia. Use a
    // conservative dynamic neighbourhood so the configured radius is real.
    let coordinateCellRadius = max 1 (int (Math.Ceiling(policy.source.stopMatch.maximumDistanceMetres / 100.0)))
    let baseStopSpatial = Dictionary<struct (int * int), ResizeArray<StopGroup>>()
    for group in nationalBase.baseStopGroups do
        for lat, lon in stopGroupPoints group do
            let key = coordinateCell lat lon
            match baseStopSpatial.TryGetValue(key) with
            | true, values -> values.Add(group)
            | _ ->
                let values = ResizeArray<StopGroup>()
                values.Add(group)
                baseStopSpatial.[key] <- values
    {
        shapeRows = shapeRows
        scratch = scratch
        policyPath = policyPath
        baseBundle = baseBundle
        outputBundle = outputBundle
        binding = binding
        policy = policy
        normalizeMatchingTime = normalizeMatchingTime
        descriptor = descriptor
        actualSourceHash = actualSourceHash
        window = window
        snapshotDate = snapshotDate
        auditDate = auditDate
        baseGtfs = baseGtfs
        baseExtensions = baseExtensions
        diagnostics = diagnostics
        stopOverrides = stopOverrides
        routeOverrides = routeOverrides
        tripOverrides = tripOverrides
        baseStopRows = nationalBase.baseStopRows
        baseRouteRows = nationalBase.baseRouteRows
        baseTripValues = nationalBase.baseTripValues
        baseCzRouteRows = nationalBase.baseCzRouteRows
        baseDates = baseDates
        baseRoutes = nationalBase.baseRoutes
        baseTrips = nationalBase.baseTrips
        baseCisByRoute = nationalBase.baseCisByRoute
        baseRoutesByCis = nationalBase.baseRoutesByCis
        basePlaceByStop = nationalBase.basePlaceByStop
        baseStopGroupById = nationalBase.baseStopGroupById
        approximateStopName = approximateStopName
        stopGroupDistance = stopGroupDistance
        stableStopNameKey = stableStopNameKey
        baseStopGroupsByName = baseStopGroupsByName
        coordinateCell = coordinateCell
        coordinateCellRadius = coordinateCellRadius
        baseStopSpatial = baseStopSpatial
    }
