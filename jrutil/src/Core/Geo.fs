// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

module JrUtil.Geo

open System

/// Great-circle distance on a 6,371 km sphere. Keep the operation order:
/// published coordinate decisions depend on the exact floating-point result.
let haversineMetres lat1 lon1 lat2 lon2 =
    let radians value = value * Math.PI / 180.0
    let dLat = radians (lat2 - lat1)
    let dLon = radians (lon2 - lon1)
    let a =
        Math.Sin(dLat / 2.0) ** 2.0
        + Math.Cos(radians lat1) * Math.Cos(radians lat2) * Math.Sin(dLon / 2.0) ** 2.0
    6371000.0 * 2.0 * Math.Atan2(Math.Sqrt(a), Math.Sqrt(1.0 - a))
