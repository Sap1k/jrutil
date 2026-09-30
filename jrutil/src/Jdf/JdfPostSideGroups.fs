// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Stable identifiers and side-group catalog for inferred posts.
module JrUtil.JdfPostSideGroups

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Security.Cryptography
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Threading
open Parquet
open Parquet.Schema
open JrUtil.JdfPostEvidenceReader
open JrUtil.JdfPostPolicyEvaluation

let internal replayTransportMode = function
    | "A" -> JdfModel.Bus
    | "E" -> JdfModel.Tram
    | "T" -> JdfModel.Trolleybus
    | "L" -> JdfModel.CableCar
    | "M" -> JdfModel.Metro
    | "P" -> JdfModel.Ferry
    | value -> invalidArg "evidencePath" $"Unsupported routed-evidence mode: {value}"

let internal stableHex (payload:string) =
    SHA256.HashData(Encoding.UTF8.GetBytes(payload))
    |> Convert.ToHexString |> fun value -> value.ToLowerInvariant()

let internal replayPhysicalLocationId stopId candidateId =
    let payload = $"{stopId}|{candidateId}"
    $"estimated:{stableHex payload}"

let internal replaySideGroupId stopId (mode:string) face =
    stableHex $"{stopId}|{mode}|{face}"

let internal replaySideLocationId stopId groupId =
    let payload = $"{stopId}|side|{groupId}"
    $"estimated-side:{stableHex payload}"

let internal replaySector (points:ReplayRoutePoint array) latitude longitude =
    if points.Length=0 then "?" else
    let centreLatitude=points |> Array.averageBy _.latitude
    let centreLongitude=points |> Array.averageBy _.longitude
    let north=(latitude-centreLatitude)*111_320.0
    let east=(longitude-centreLongitude)*111_320.0*Math.Cos(centreLatitude*Math.PI/180.0)
    if Math.Sqrt(north*north+east*east)<2.0 then "?" else
    let index=int(Math.Round(((Math.Atan2(east,north)*180.0/Math.PI+360.0)%360.0)/45.0))%8
    [|"N";"NE";"E";"SE";"S";"SW";"W";"NW"|].[index]

let internal sideGroupCatalog
        (policy:JdfPostInferencePolicy.PostInferencePolicyV2)
        (routePointsByStop:Map<int64,ReplayRoutePoint array>)
        (decisions:ReplayDecision array)
        : JdfPostInference.GlobalPostSideGroup array =
    decisions
    |> Array.choose(fun value ->
        match value.resolution,value.corridorFaceId,value.representativeCandidateId,
              value.latitude,value.longitude with
        | "Side",Some face,Some hypothesisId,Some latitude,Some longitude ->
            Some(value.stopId,value.mode,face,value.movementFamilyId,
                 hypothesisId,latitude,longitude)
        | _ -> None)
    |> Array.groupBy(fun (stopId,mode,face,_,_,_,_) -> stopId,mode,face)
    |> Array.choose(fun ((stopId,mode,face),values) ->
        let members=values |> Array.distinctBy(fun (_,_,_,_,candidateId,_,_) -> candidateId)
        let distance (_,_,_,_,_,leftLat,leftLon) (_,_,_,_,_,rightLat,rightLon) =
            let dlat=(leftLat-rightLat)*111_320.0
            let dlon=(leftLon-rightLon)*111_320.0*Math.Cos((leftLat+rightLat)*Math.PI/360.0)
            Math.Sqrt(dlat*dlat+dlon*dlon)
        let compactness =
            seq { for left in members do for right in members do yield distance left right }
            |> Seq.append [0.0] |> Seq.max
        if compactness>policy.sideGroups.maximumCompactnessMetres then None else
        let medoid=members |> Array.minBy(fun candidate ->
            members |> Array.sumBy(distance candidate),
            (let _,_,_,_,candidateId,_,_=candidate in candidateId))
        let _,_,_,_,representative,latitude,longitude=medoid
        let groupId=replaySideGroupId stopId mode face
        let group:JdfPostInference.GlobalPostSideGroup = {
            sideGroupId=groupId
            stopId=stopId;mode=mode;corridorFaceId=face
            sector=replaySector
                (routePointsByStop |> Map.tryFind stopId |> Option.defaultValue [||])
                latitude longitude
            representativeHypothesisId=representative
            memberHypothesisIds=
                members |> Array.map(fun (_,_,_,_,candidateId,_,_) -> candidateId) |> Array.sort
            latitude=latitude;longitude=longitude;compactnessMetres=compactness
            support=values |> Array.distinctBy(fun (_,_,_,family,_,_,_) -> family) |> Array.length }
        Some group)
    |> Array.groupBy(fun value -> value.stopId,value.mode)
    |> Array.collect(fun (_,groups) ->
        groups
        |> Array.sortBy(fun value -> -value.support,value.corridorFaceId)
        |> Array.mapi(fun index value -> index,value)
        |> Array.choose(fun (index,value) ->
            if index<policy.sideGroups.maximumOrdinaryGroups
               || value.support>=policy.sideGroups.additionalGroupMinimumContexts
            then Some value else None))
    |> Array.sortBy(fun value -> value.stopId,value.mode,value.sideGroupId)
