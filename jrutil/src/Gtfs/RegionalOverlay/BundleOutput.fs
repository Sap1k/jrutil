// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module internal JrUtil.RegionalOverlay.BundleOutput

open System.Collections.Generic

open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay

/// Resolved overlay state and the output facts accumulated while writing.
/// Later writers read the IDs earlier ones marked as used.
type Context = {
    prepared: InputPreparation.Result
    projection: Projection.Result
    source: SourceAnalysis.Result
    matches: TripMatching.Result
    gtfsOutput: string
    extensionsOutput: string
    usedStopIds: HashSet<string>
    usedRouteIds: HashSet<string>
    usedAgencyIds: HashSet<string>
}

/// IDS JMK fare zones claimed for national stop places.
type StopZones = {
    regionalZonesByPlace: Dictionary<string, string array>
    outputZoneIdentity: string -> string
    isUsedPlace: string -> bool
}

/// Output transfers and their provenance, plus the slice lookup reports reuse.
type TransferOutput = {
    rows: ResizeArray<CsvRow>
    provenance: ResizeArray<string array>
    slicesForBinding: MatchBinding -> (TripSlice * DateSet.Dates) array
}
