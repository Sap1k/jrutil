// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.GeoData.ExternalCsv

open System
open System.Globalization
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

type OtherStopRow = {
    Name: string
    Lat: float
    Lon: float
    Region: string option
    Country: string option
    Precision: string option
    Source: string option
}

let otherStopRowsForJdfMatch (otherStopRows: OtherStopRow seq) =
    otherStopRows
    |> Seq.map (fun s ->
        let precision =
            match s.Precision |> Option.map (fun value -> value.Trim().ToUpperInvariant()) with
            | None | Some "" | Some "S" -> StopPrecise
            | Some "T" -> TownPrecise
            | Some value -> invalidArg "precision" $"Unknown external geodata precision '{value}'"
        {
        name = s.Name
        data = {
            regionId = s.Region
            country = s.Country
            point = pointWgs84ToEtrs89Ex
                 <| wgs84Factory.CreatePoint(Coordinate(s.Lon, s.Lat))
            precision = precision
            source = s.Source
        }
    })
    |> Seq.toArray

let otherStopsForJdfMatch (otherStops: OtherStops) =
    otherStops.Rows
    |> Seq.map (fun row -> {
        Name = row.Name; Lat = row.Lat; Lon = row.Lon
        Region = row.Region; Country = row.Country; Precision = None
        Source = Some "external:legacy"
    })
    |> otherStopRowsForJdfMatch

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

    let optionalText value =
        if String.IsNullOrWhiteSpace(value) then None else Some value
    let parseFloat (file: string) line name (value: string) =
        match Double.TryParse(value, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, result -> result
        | _ -> invalidArg "path" $"Invalid {name} in {file} row {line}: '{value}'"
    files
    |> Seq.collect (fun file ->
        CsvFile.Parse(File.ReadAllText(file), hasHeaders = false).Rows
        |> Seq.mapi (fun index row ->
            let columns = row.Columns
            if columns.Length <> 5 && columns.Length <> 6 then
                invalidArg "path" $"External geodata row must have five or six columns: {file} row {index + 1}"
            let lat = parseFloat file (index + 1) "latitude" columns.[1]
            let lon = parseFloat file (index + 1) "longitude" columns.[2]
            if not (Double.IsFinite(lat)) || lat < -90.0 || lat > 90.0
            then invalidArg "path" $"Invalid latitude in {file} row {index + 1}: '{columns.[1]}'"
            if not (Double.IsFinite(lon)) || lon < -180.0 || lon > 180.0
            then invalidArg "path" $"Invalid longitude in {file} row {index + 1}: '{columns.[2]}'"
            {
                Name = columns.[0]
                Lat = lat
                Lon = lon
                Region = optionalText columns.[3]
                Country = optionalText columns.[4]
                Precision = if columns.Length = 6 then optionalText columns.[5] else None
                Source = Some $"external:{Path.GetFileNameWithoutExtension(file)}"
            }))
    |> Seq.distinctBy (fun s -> s.Name, s.Lat, s.Lon, s.Region, s.Country, s.Precision)
    |> Seq.toArray

let otherStopsFromPathForJdfMatch path =
    otherStopsFromPath path
    |> otherStopRowsForJdfMatch
