// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.GeoData.Common

open NetTopologySuite.Geometries
open ProjNet.Geometries
open ProjNet.IO.CoordinateSystems
open ProjNet.CoordinateSystems
open ProjNet.CoordinateSystems.Transformations

// Common coordinate systems
let wgs84 = (CoordinateSystemWktReader.Parse(
    """GEOGCS["GCS_WGS_1984",
        DATUM["D_WGS_1984",
            SPHEROID["WGS_1984",6378137.0,298.257223563]],
        PRIMEM["Greenwich",0.0],
        UNIT["Degree",0.0174532925199433]]
    """) :?> GeographicCoordinateSystem)
let etrs89Ex = (CoordinateSystemWktReader.Parse(
    """PROJCS["ETRS89-extended / LAEA Europe",
        GEOGCS["ETRS89",
            DATUM["European_Terrestrial_Reference_System_1989",
                SPHEROID["GRS 1980",6378137,298.257222101,
                    AUTHORITY["EPSG","7019"]],
                TOWGS84[0,0,0,0,0,0,0],
                AUTHORITY["EPSG","6258"]],
            PRIMEM["Greenwich",0,
                AUTHORITY["EPSG","8901"]],
            UNIT["degree",0.0174532925199433,
                AUTHORITY["EPSG","9122"]],
            AUTHORITY["EPSG","4258"]],
        PROJECTION["Lambert_Azimuthal_Equal_Area"],
        PARAMETER["latitude_of_center",52],
        PARAMETER["longitude_of_center",10],
        PARAMETER["false_easting",4321000],
        PARAMETER["false_northing",3210000],
        UNIT["metre",1,
            AUTHORITY["EPSG","9001"]],
        AUTHORITY["EPSG","3035"]]
    """) :?> ProjectedCoordinateSystem)

let wgs84Srid = 4326
let etrs89ExSrid = 3035

let wgs84Factory = GeometryFactory(PrecisionModel(), wgs84Srid)
let etrs89ExFactory = GeometryFactory(PrecisionModel(), etrs89ExSrid)

let coordTransformFactory = CoordinateTransformationFactory()
let wgs84ToEtrs89Ex =
    coordTransformFactory.CreateFromCoordinateSystems(wgs84, etrs89Ex)
     .MathTransform
// ProjNet initializes a concatenated transform's inverse lazily and mutates
// internal lists while doing so. Initialize it once during module startup;
// calling Inverse() concurrently from fix-jdf workers is not thread-safe.
let etrs89ExToWgs84 = wgs84ToEtrs89Ex.Inverse()

let transformCoordinates (transform: MathTransform) =
    Array.map (fun (c: Coordinate) ->
        let out = transform.Transform([| c.X; c.Y |])
        Coordinate(out.[0], out.[1]))

let transformPolygon sourceSrid targetSrid
                     (transform: MathTransform)
                     (polygon: Polygon) =
    assert (polygon.SRID = sourceSrid)
    let tcs = transformCoordinates transform
    let shell = LinearRing(tcs polygon.Shell.Coordinates)
    let holes =
        polygon.Holes
        |> Array.map (fun h -> LinearRing(tcs h.Coordinates))
    let outPolygon = Polygon(shell, holes)
    outPolygon.SRID <- targetSrid
    outPolygon.UserData <- polygon.UserData
    outPolygon

let transformPoint sourceSrid targetSrid
                   (transform: MathTransform)
                   (point: Point) =
    assert (point.SRID = sourceSrid)

    let outCoord = transform.Transform([|
        point.Coordinate.X; point.Coordinate.Y |])
    let outPoint = Point(outCoord.[0], outCoord.[1])
    outPoint.SRID <- targetSrid
    outPoint.UserData <- point.UserData
    outPoint

let polygonWgs84ToEtrs89Ex p =
    transformPolygon wgs84Srid etrs89ExSrid wgs84ToEtrs89Ex p
let pointWgs84ToEtrs89Ex p =
    transformPoint wgs84Srid etrs89ExSrid wgs84ToEtrs89Ex p
let pointEtrs89ExToWgs84 p =
    transformPoint etrs89ExSrid wgs84Srid etrs89ExToWgs84 p

let pointsRadius (points: Point array) =
    let avgX = points |> Array.averageBy (fun p -> p.X)
    let avgY = points |> Array.averageBy (fun p -> p.Y)
    let avgPoint = Point(avgX, avgY)
    points |> Array.map (fun p -> p.Distance(avgPoint)) |> Array.max
