// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

/// Stop-name and route-label comparison for overlay matching.
module internal JrUtil.RegionalOverlay.Names

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let normalizeName (value: string) =
    value.ToLowerInvariant()
    |> Seq.map (fun value -> if Char.IsLetterOrDigit(value) then value else ' ')
    |> Array.ofSeq
    |> String
    |> fun value -> value.Split(' ', StringSplitOptions.RemoveEmptyEntries)

/// One word of a stop name. `dotted` marks a word written as an abbreviation
/// ("žel.", "n."), which may stand for any word it begins.
[<Struct>]
type private NameToken = { text: string; dotted: bool }

let private nameTokens (value: string) =
    let tokens = ResizeArray<NameToken>()
    let word = Text.StringBuilder()
    let flush dotted =
        if word.Length > 0 then
            tokens.Add({ text = word.ToString(); dotted = dotted })
            word.Clear() |> ignore
    for character in value.ToLowerInvariant() do
        if Char.IsLetterOrDigit(character) then word.Append(character) |> ignore
        else flush (character = '.')
    flush false
    tokens.ToArray()

/// CIS writes a junction with its distance from the village: "Miroslav,rozc.4.0".
let private junctionDistance = Regex(@"\s*\d+[.,]\d+\s*$", RegexOptions.CultureInvariant)

/// The comma-separated parts of a name (municipality, part, place), without
/// the empty ones of "Obec,,místo".
let private nameSegments (value: string) =
    value.Split(',')
    |> Array.map (fun segment ->
        let segment = segment.Trim()
        if segment.Contains("rozc", StringComparison.OrdinalIgnoreCase) then junctionDistance.Replace(segment, "") else segment)
    |> Array.map nameTokens
    |> Array.filter (fun tokens -> tokens.Length > 0)

/// Expansions that a prefix cannot find: acronyms and the like. The shared
/// vocabulary is `StopMatcherSynonyms.txt`; a line's first member and every
/// member ending with a dot abbreviate the others.
let private overlayExpansions = [|
    "žst", "železniční stanice"
    "zd", "zemědělské družstvo"
    "mš", "mateřská škola"
    "zš", "základní škola"
    "kd", "kulturní dům"
|]

let private expansions =
    lazy (
        let result = Dictionary<string, ResizeArray<string array>>(StringComparer.Ordinal)
        let add (abbreviation: string) (expansion: string) =
            let key = abbreviation.Trim().TrimEnd('.')
            let words = nameTokens expansion |> Array.map (fun token -> token.text)
            // One-word expansions of a dotted abbreviation are found as prefixes already.
            if key <> "" && not (key.Contains(' ')) && words.Length > 0 && not (words.Length = 1 && words.[0] = key) then
                match result.TryGetValue(key) with
                | true, values -> values.Add(words)
                | _ -> result.[key] <- ResizeArray([| words |])
        for abbreviation, expansion in overlayExpansions do add abbreviation expansion
        let path = JrUtil.GeoData.StopMatcher.synonymsFile
        if File.Exists(path) then
            for line in File.ReadLines(path) do
                let members =
                    line.Split('#').[0].ToLowerInvariant().Split(',')
                    |> Array.map (fun item -> item.Trim())
                    |> Array.filter (fun item -> item <> "")
                if members.Length > 1 then
                    let abbreviations = members |> Array.indexed |> Array.filter (fun (index, item) -> index = 0 || item.EndsWith('.')) |> Array.map snd
                    let full = members |> Array.filter (fun item -> not (item.EndsWith('.')))
                    for abbreviation in abbreviations do
                        for expansion in full do
                            if expansion <> abbreviation then add abbreviation expansion
        result)

let private sameWord (left: NameToken) (right: NameToken) =
    left.text = right.text
    || (left.dotted && right.text.StartsWith(left.text, StringComparison.Ordinal))
    || (right.dotted && left.text.StartsWith(right.text, StringComparison.Ordinal))
    || (min left.text.Length right.text.Length >= 3
        && (left.text.StartsWith(right.text, StringComparison.Ordinal) || right.text.StartsWith(left.text, StringComparison.Ordinal)))

/// How many words differ in spelling when the two names say the same, else None.
let private nameDifference (left: NameToken array) (right: NameToken array) =
    let known = Dictionary<struct (int * int), int option>()
    let expandsTo (short: NameToken array) index (long: NameToken array) position =
        match expansions.Value.TryGetValue(short.[index].text) with
        | true, values ->
            values
            |> Seq.filter (fun words ->
                position + words.Length <= long.Length
                && words |> Array.indexed |> Array.forall (fun (offset, word) -> long.[position + offset].text = word))
            |> Seq.map (fun words -> words.Length)
            |> Seq.toArray
        | _ -> [||]
    let rec from i j =
        if i = left.Length && j = right.Length then Some 0
        elif i = left.Length || j = right.Length then None
        else
            match known.TryGetValue(struct (i, j)) with
            | true, value -> value
            | _ ->
                let word =
                    if sameWord left.[i] right.[j] then
                        from (i + 1) (j + 1) |> Option.map (fun rest -> rest + (if left.[i].text = right.[j].text then 0 else 1))
                    else None
                let leftExpanded = expandsTo left i right j |> Array.choose (fun length -> from (i + 1) (j + length) |> Option.map ((+) 1))
                let rightExpanded = expandsTo right j left i |> Array.choose (fun length -> from (i + length) (j + 1) |> Option.map ((+) 1))
                let options = Seq.concat [ Option.toArray word; leftExpanded; rightExpanded ] |> Seq.toArray
                let value = if options.Length = 0 then None else Some (Array.min options)
                known.[struct (i, j)] <- value
                value
    from 0 0

/// Rank two stop names that say the same place, lower being closer: 0 for the
/// same words, more for abbreviations spelled out and for leading locality
/// segments ("Brno,Bystrc,ZOO" against "ZOO") one side leaves out. None when
/// the names differ.
let stopNameMatchRank (leftRaw: string) (rightRaw: string) =
    let left, right = nameSegments leftRaw, nameSegments rightRaw
    let whole (segments: NameToken array array) = Array.concat segments
    match nameDifference (whole left) (whole right) with
    | Some difference -> Some difference
    | None ->
        // The longer name may lead with locality segments the other leaves out.
        let withoutLocality (longer: NameToken array array) (shorter: NameToken array array) =
            seq { 1 .. longer.Length - 1 }
            |> Seq.choose (fun dropped ->
                nameDifference (whole longer.[dropped..]) (whole shorter)
                |> Option.map (fun difference -> difference + (longer.[.. dropped - 1] |> Array.sumBy Array.length)))
            |> Seq.toArray
        let ranks = Array.append (withoutLocality left right) (withoutLocality right left)
        if ranks.Length = 0 then None else Some (Array.min ranks)

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
