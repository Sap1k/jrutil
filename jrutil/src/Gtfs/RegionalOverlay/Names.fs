// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

/// Stop-name and route-label comparison for overlay matching.
module internal JrUtil.RegionalOverlay.Names

open System
open JrUtil.Hashing
open System.Collections.Generic
open System.Globalization
open System.IO
open System.IO.Compression
open System.Text.RegularExpressions
open System.Security.Cryptography
open System.Text
open System.Text.Json
open Microsoft.VisualBasic.FileIO
open NodaTime
open NodaTime.Text
open JrUtil.GtfsModel
open JrUtil.RegionalOverlay.Types
open JrUtil.RegionalOverlay.Model

let normalizeName (value: string) =
    value.ToLowerInvariant()
    |> Seq.map (fun value -> if Char.IsLetterOrDigit(value) then value else ' ')
    |> Array.ofSeq
    |> String
    |> fun value -> value.Split(' ', StringSplitOptions.RemoveEmptyEntries)

let internal canonicalStopNameTokens (value: string) =
    let tokens = normalizeName value
    let expanded = ResizeArray<string>()
    let mutable index = 0
    while index < tokens.Length do
        let token = tokens.[index]
        let next = if index + 1 < tokens.Length then tokens.[index + 1] else ""
        match token, next with
        | "žel", "st" ->
            expanded.Add("železniční")
            expanded.Add("stanice")
            index <- index + 2
        | "čerp", "st" ->
            expanded.Add("čerpací")
            expanded.Add("stanice")
            index <- index + 2
        | "obch", "stř" ->
            expanded.Add("obchodní")
            expanded.Add("středisko")
            index <- index + 2
        | "obú", _ ->
            expanded.Add("obecní")
            expanded.Add("úřad")
            index <- index + 1
        | "nám", _ -> expanded.Add("náměstí"); index <- index + 1
        | "rozc", _ -> expanded.Add("rozcestí"); index <- index + 1
        | "rest", _ -> expanded.Add("restaurace"); index <- index + 1
        | "zast", _ -> expanded.Add("zastávka"); index <- index + 1
        | "dol", _ -> expanded.Add("dolní"); index <- index + 1
        | "hor", _ -> expanded.Add("horní"); index <- index + 1
        | "nem", _ -> expanded.Add("nemocnice"); index <- index + 1
        | "kult", _ -> expanded.Add("kulturní"); index <- index + 1
        | "mech", _ -> expanded.Add("mechanizační"); index <- index + 1
        | "stř", _ -> expanded.Add("středisko"); index <- index + 1
        | "záv", _ -> expanded.Add("závod"); index <- index + 1
        | "n", _ -> expanded.Add("nad"); index <- index + 1
        | _ -> expanded.Add(token); index <- index + 1
    expanded.ToArray()

let stopNameMatchRank (leftRaw: string) (rightRaw: string) =
    let left = canonicalStopNameTokens leftRaw
    let right = canonicalStopNameTokens rightRaw
    let localitySuffix (shorter: string array) (longerRaw: string) =
        let comma = longerRaw.IndexOf(',')
        comma >= 0 && canonicalStopNameTokens longerRaw.[comma + 1..] = shorter
    let prefixEquivalent (left: string array) (right: string array) =
        left.Length = right.Length
        && Array.zip left right
           |> Array.forall (fun (a, b) ->
               a = b
               || (min a.Length b.Length >= 3 && (a.StartsWith(b, StringComparison.Ordinal) || b.StartsWith(a, StringComparison.Ordinal))))
    if left = right then Some 0
    elif localitySuffix left rightRaw || localitySuffix right leftRaw then Some (abs (left.Length - right.Length))
    elif prefixEquivalent left right then
        Some (Array.zip left right |> Array.sumBy (fun (a, b) -> if a = b then 0 else 1))
    else None

let compatibleModeClasses (left: string) (right: string) =
    left = right
    || (Set.ofList [ "bus"; "trolleybus" ] |> fun road -> road.Contains(left) && road.Contains(right))

let structurallyCompatibleRouteLabels (leftRaw: string) (rightRaw: string) =
    let normalize (value: string) =
        value.ToUpperInvariant()
        |> Seq.filter Char.IsLetterOrDigit
        |> Array.ofSeq
        |> String
    let left, right = normalize leftRaw, normalize rightRaw
    let prefixVariant (shorter: string) (longer: string) =
        shorter.Length = 1
        && Char.IsLetter(shorter.[0])
        && longer.StartsWith(shorter, StringComparison.Ordinal)
        && longer.[shorter.Length..] |> Seq.forall Char.IsDigit
    left = right || (left <> "" && right <> "" && (prefixVariant left right || prefixVariant right left))

let haversineMetres = JrUtil.Geo.haversineMetres
