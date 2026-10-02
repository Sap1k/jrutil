// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Optimized hot paths compared against frozen copies of the implementations
/// they replaced, so speedups cannot change converter output.
module JrUtil.Tests.PerformanceEquivalenceTests

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open Microsoft.VisualStudio.TestTools.UnitTesting
open NodaTime

open JrUtil
open JrUtil.GtfsModel

module private Reference =
    let normalizedName (value: string) =
        let transliterated =
            value
                .Replace("ß", "ss")
                .Replace("ẞ", "SS")
                .Replace("Ł", "L")
                .Replace("ł", "l")
                .Replace("Ø", "O")
                .Replace("ø", "o")
                .Replace("Æ", "AE")
                .Replace("æ", "ae")
        let decomposed = transliterated.Normalize(NormalizationForm.FormD)
        let withoutMarks =
            decomposed
            |> Seq.filter (fun character ->
                Globalization.CharUnicodeInfo.GetUnicodeCategory(character)
                <> Globalization.UnicodeCategory.NonSpacingMark)
            |> Seq.toArray
            |> String
        Regex.Replace(withoutMarks, @"[^\p{L}\p{N}]+", " ")
            .Trim().ToUpperInvariant()

    let railwayNameCore (value: string) =
        let withoutOperationalQualifier =
            Regex.Replace(value, @"\s*\([^)]*\)\s*$", "")
        let removable =
            Set.ofList [
                "Bf"; "Fbf"; "Gr"; "Hp"; "Hst"; "N"; "Nz"; "Pzs"; "S"; "St";
                "Z"; "Zast"; "Zastavka"
            ]
            |> Set.map normalizedName
        let tokens =
            (normalizedName withoutOperationalQualifier)
                .Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
        let rec trimSuffix values =
            match List.tryLast values with
            | Some token when
                Set.contains token removable
                || Regex.IsMatch(token, @"^R[0-9]+$") ->
                values |> List.take (values.Length - 1) |> trimSuffix
            | _ -> values
        match trimSuffix tokens with
        | [] -> normalizedName value
        | values -> String.concat " " values

    let nameSimilarity (left: string) (right: string) =
        let leftFull = normalizedName left
        let rightFull = normalizedName right
        let leftCore = railwayNameCore left
        let rightCore = railwayNameCore right
        let compact (value: string) = value.Replace(" ", "")
        [|
            CzPttCoordinates.editSimilarity leftFull rightFull
            CzPttCoordinates.editSimilarity leftCore rightCore
            CzPttCoordinates.editSimilarity (compact leftCore) (compact rightCore)
        |]
        |> Array.max

    let fuzzyQualifierCompatible (expected: string) (candidate: string) =
        let tokens value =
            (normalizedName value).Split(' ', StringSplitOptions.RemoveEmptyEntries)
        let required =
            tokens expected
            |> Array.filter (fun token ->
                Set.contains token CzPttCoordinates.protectedFuzzyNameTokens)
        let available = tokens candidate
        required
        |> Array.forall (fun requiredToken ->
            available
            |> Array.exists (fun candidateToken ->
                candidateToken = requiredToken
                || candidateToken.StartsWith(requiredToken, StringComparison.Ordinal)))

    let deduplicateCalendar (feed: GtfsFeed) =
        let allCalEntries = feed.calendar |> Option.defaultValue [||]
        let allCalExcs = feed.calendarExceptions |> Option.defaultValue [||]
        let serviceIds =
            Set.union
                (allCalEntries |> Array.map (fun ce -> ce.id) |> Set)
                (allCalExcs |> Array.map (fun c -> c.id) |> Set)
        let calById = allCalEntries |> Array.map (fun ce -> ce.id, ce) |> Map
        let excsById = allCalExcs |> Array.groupBy (fun ce -> ce.id) |> Map
        serviceIds
        |> Seq.map (fun si ->
            let calEntry = calById |> Map.tryFind si
            let calExcs = excsById |> Map.tryFind si |> Option.defaultValue [||]
            (calEntry |> Option.map (fun ce -> { ce with id = "" }),
             calExcs |> Array.map (fun ce -> { ce with id = "" }) |> Array.sort),
            si)
        |> Seq.groupBy fst
        |> Seq.mapi (fun i (_, items) ->
            let items = items |> Seq.toArray
            let (reprCalEntry, reprCalExcs), _ = items |> Array.head
            let bitmapStr =
                reprCalEntry
                |> Option.map (fun ce ->
                    ce.weekdayService |> Array.map (fun b -> if b then '1' else '0') |> String)
                |> Option.defaultValue "exc"
            let newId = $"gtfs:service:{bitmapStr}:{i}"
            items |> Array.map snd,
            reprCalEntry |> Option.map (fun ce -> { ce with id = newId }),
            reprCalExcs |> Array.map (fun ce -> { ce with id = newId }))
        |> Seq.toArray

let private names =
    let known = [|
        "Praha hl.n."; "Praha-Smíchov"; "Praha-Smíchov sev.n."; "Brno hl.n."
        "Ostrava-Svinov"; "Ostrava střed"; "Ostrava Stodolní"; "Plzeň hl.n."
        "Głuchołazy"; "Głuchołazy Fabryka Mebli"; "Głuchołazy Zdrój"
        "Wrocław Główny"; "Kraków Płaszów"; "Katowice Zachód"; "Gdańsk Wschód"
        "Bärenstein (Annaberg)"; "Bärenstein"; "Dresden Hbf"; "Bad Schandau Bf"
        "Zittau Süd"; "Görlitz Nord"; "Wien Mitte"; "Linz/Donau Hbf"; "Ærøskøbing"
        "Lysá nad Labem"; "Lysá n.L. zast."; "Kolín seř.n."; "Kolín R12"
        "Benešov u Prahy"; "Benešov u Prahy zastávka"; "Most"; "Most město"
        "Česká Třebová"; "Ústí nad Labem západ"; "Ústí nad Labem-Střekov"
        "Žilina"; "Bratislava hl.st."; "Bratislava-Petržalka"; "Košice juh"
        "Hp"; "St"; "R5"; ""; "  "; "Ž"; "(Pražská)"; "A (B)"; "Nové Město n.M."
    |]
    let random = Random(1729)
    let alphabet = "abcdefghijklmnopqrstuvwxyzěščřžýáíéůúňťďŁłßÄÖÜäöü -.()/0123456789"
    let generated =
        Array.init 120 (fun _ ->
            String(Array.init (random.Next(0, 24)) (fun _ -> alphabet.[random.Next(alphabet.Length)])))
    Array.append known generated

[<TestClass>]
type PerformanceEquivalenceTests() =
    [<TestMethod>]
    member _.``CZPTT name normalization matches the previous implementation``() =
        for name in names do
            Assert.AreEqual(Reference.normalizedName name, CzPttCoordinates.normalizedName name, name)
            Assert.AreEqual(Reference.railwayNameCore name, CzPttCoordinates.railwayNameCore name, name)

    [<TestMethod>]
    member _.``CZPTT prepared fuzzy matching matches the previous implementation``() =
        let prepared = names |> Array.map CzPttCoordinates.prepareName
        for leftIndex in 0 .. names.Length - 1 do
            let left = names.[leftIndex]
            let required = CzPttCoordinates.requiredQualifierTokens prepared.[leftIndex]
            for rightIndex in 0 .. names.Length - 1 do
                let right = names.[rightIndex]
                let expected = Reference.nameSimilarity left right
                Assert.AreEqual(expected, CzPttCoordinates.nameSimilarity left right, $"{left} / {right}")
                Assert.AreEqual(
                    Reference.fuzzyQualifierCompatible left right,
                    CzPttCoordinates.preparedQualifierCompatible required prepared.[rightIndex],
                    $"{left} / {right}")
                for minimum in [ 0.; 0.58; 0.9 ] do
                    let bounded =
                        CzPttCoordinates.preparedNameSimilarityAtLeast
                            minimum prepared.[leftIndex] prepared.[rightIndex]
                    Assert.AreEqual(
                        (if expected >= minimum then Some expected else None),
                        bounded,
                        $"{left} / {right} at {minimum}")

    [<TestMethod>]
    member _.``Calendar deduplication matches the previous implementation``() =
        let random = Random(31)
        let start = LocalDate(2025, 12, 14)
        // Long shared exception prefixes are the case the old array key hashed badly.
        let sharedPrefix =
            Array.init 40 (fun day -> start.PlusDays(day * 2), ServiceRemoved)
        let services =
            Array.init 300 (fun index ->
                let id = $"s{random.Next(1000):D3}-{index}"
                let entry =
                    if random.Next(3) = 0 then None
                    else
                        Some {
                            id = id
                            weekdayService = Array.init 7 (fun _ -> random.Next(4) > 0)
                            startDate = start.PlusDays(random.Next(2))
                            endDate = start.PlusDays(300)
                        }
                let tail =
                    Array.init (random.Next(0, 3)) (fun _ ->
                        start.PlusDays(100 + random.Next(6)),
                        if random.Next(2) = 0 then ServiceAdded else ServiceRemoved)
                    |> Array.distinctBy fst
                let exceptions =
                    (if random.Next(2) = 0 then Array.append sharedPrefix tail else tail)
                    |> Array.map (fun (date, kind) -> { id = id; date = date; exceptionType = kind })
                    // Shuffle so both implementations must canonicalize the order.
                    |> Array.sortBy (fun _ -> random.Next())
                entry, exceptions)
        let feed: GtfsFeed = {
            agencies = [||]; stops = [||]; routes = [||]; trips = [||]; stopTimes = [||]
            shapes = None
            calendar = Some (services |> Array.choose fst)
            calendarExceptions = Some (services |> Array.collect snd)
            feedInfo = None; transfers = None; czRoutes = None; czTrips = None
            czStops = None; czStopZones = None; czTripStopZones = None
        }
        let reference = Reference.deduplicateCalendar feed
        let actual = Gtfs.deduplicateCalendar feed
        Assert.IsTrue(reference.Length < services.Length, "fixture must contain duplicates")
        Assert.IsTrue((reference |> Array.choose (fun (_, entry, _) -> entry)) = actual.calendar.Value)
        Assert.IsTrue(
            (reference |> Array.collect (fun (_, _, exceptions) -> exceptions)) = actual.calendarExceptions.Value)
