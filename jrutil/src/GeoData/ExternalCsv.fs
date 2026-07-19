// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.GeoData.ExternalCsv

open System
open System.IO
open FSharp.Data
open NetTopologySuite.Geometries

open JrUtil.GeoData.Common
open JrUtil.GeoData.StopMatcher
open JrUtil.JdfModel
open JrUtil.JdfFixups

#nowarn "0058"

type OtherStops = CsvProvider<
    HasHeaders = false,
    Schema = "name(string), lat(float), lon(float), region(string option), country(string option)">
type CzRailStops = CsvProvider<
    HasHeaders = false,
    Schema = "sr70(string), name(string), lat(float), lon(float)">

let otherStopRowsForJdfMatch (otherStopRows: OtherStops.Row seq) =
    otherStopRows
    |> Seq.map (fun s -> {
        name = s.Name
        data = {
            regionId = s.Region
            country = s.Country
            point = pointWgs84ToEtrs89Ex
                 <| wgs84Factory.CreatePoint(Coordinate(s.Lon, s.Lat))
            precision = StopPrecise
        }
    })
    |> Seq.toArray

let otherStopsForJdfMatch (otherStops: OtherStops) =
    otherStops.Rows |> otherStopRowsForJdfMatch

let private otherStopFiles path =
    if File.Exists(path) then
        [| Path.GetFullPath(path) |]
    elif Directory.Exists(path) then
        Directory.EnumerateFiles(path, "*.csv", SearchOption.AllDirectories)
        |> Seq.sortWith (fun left right ->
            StringComparer.Ordinal.Compare(
                Path.GetRelativePath(path, left).Replace('\\', '/'),
                Path.GetRelativePath(path, right).Replace('\\', '/')))
        |> Seq.toArray
    else
        invalidArg "path" $"External geodata path does not exist: {path}"

let otherStopsFromPath path =
    let files = otherStopFiles path
    if files.Length = 0 then
        invalidArg "path" $"External geodata directory contains no CSV files: {path}"

    files
    |> Seq.collect (fun file -> OtherStops.Parse(File.ReadAllText(file)).Rows)
    |> Seq.distinctBy (fun s -> s.Name, s.Lat, s.Lon, s.Region, s.Country)
    |> Seq.toArray

let otherStopsFromPathForJdfMatch path =
    otherStopsFromPath path
    |> otherStopRowsForJdfMatch
