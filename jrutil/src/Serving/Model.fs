// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System.Collections.Generic
open System

/// A finalized compiler result. Writers may only serialize relations held by
/// this model; provider-specific matching state and diagnostic scores are not
/// part of the production boundary.
module Model =
    type TripBinding = {
        binding_id: string; source_id: string; trip_namespace: string; source_trip_id: string
        trip_id: string; service_id: string; valid_from: DateOnly; valid_to: DateOnly
        binding_status: string; scheduled_start: Nullable<int>; scheduled_end: Nullable<int>
        source_route_id: string; source_direction_id: string; source_start_location_id: string
        source_end_location_id: string; source_block_id: string; source_run_id: string
        source_duty_id: string; call_pattern_sha256: string; variant_key: string
    }

    /// Native facts emitted alongside the GTFS sink, owned by the compilation scratch scope.
    type NativeCallArtifacts = {
        summaries: string
        sourceCalls: string
        tripFacts: string
        transferSequences: IDictionary<struct(string * int64), int>
    }

    type Row = IDictionary<string, obj>

    type RelationRows = {
        schema: Schema.Relation
        rows: seq<Row>
    }

    type Finalized = {
        relations: Map<string, RelationRows>
        contractValid: bool
        publicationEligible: bool
    }

    let empty contractValid publicationEligible = {
        relations = Map.empty
        contractValid = contractValid
        publicationEligible = publicationEligible
    }

    let withRelation (schema: Schema.Relation) (rows: seq<Row>) (value: Finalized) =
        { value with
            relations =
                value.relations
                |> Map.add schema.name { schema = schema; rows = rows } }
