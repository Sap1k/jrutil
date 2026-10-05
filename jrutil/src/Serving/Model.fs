// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System.Collections.Generic
open System

/// A finalized compiler result. Writers may only serialize relations held by
/// this model; provider-specific matching state and diagnostic scores are not
/// part of the production boundary.
module Model =
    /// One source key of a public trip, valid on the trip's dates within
    /// [validFrom, validTo]. `bindingKey` is a compiler-internal join key.
    type TripBinding = {
        bindingKey: string; sourceId: string; keyNamespace: string; identifier: string
        tripId: string; serviceId: string; validFrom: DateOnly; validTo: DateOnly
        method: string
    }

    /// Native facts emitted alongside the GTFS sink, owned by the compilation scratch scope.
    type NativeCallArtifacts = {
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
