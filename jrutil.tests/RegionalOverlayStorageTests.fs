namespace JrUtil.Tests

open System
open System.IO
open System.Text
open System.Security.Cryptography
open Microsoft.VisualStudio.TestTools.UnitTesting
open JrUtil.RegionalOverlay
open JrUtil.RegionalOverlay.Model
open JrUtil.RegionalOverlay.Support

[<TestClass>]
type RegionalOverlayStorageTests() =
    [<TestMethod>]
    member _.``Packed dates preserve canonical bits and identifiers across GVD boundaries``() =
        let random = Random(712)
        let pool = DateSet.Pool()
        for year in 2024 .. 2028 do
            let window = gvdWindow year
            for _ in 1 .. 20 do
                let values = Array.init window.dates.Length (fun _ -> random.Next(3) = 0)
                let original = Array.copy values
                let bytes = Array.zeroCreate<byte> ((values.Length + 7) / 8)
                for i in 0 .. values.Length - 1 do
                    if values.[i] then bytes.[i / 8] <- bytes.[i / 8] ||| (1uy <<< (i % 8))
                let key = Convert.ToHexString(SHA256.HashData(bytes)).ToLowerInvariant()
                let packed = pool.Intern values
                values.[0] <- not values.[0]
                CollectionAssert.AreEqual(original, Seq.toArray packed)
                Assert.AreEqual(key, packed.Key)
                Assert.AreEqual("overlay:service:" + key.Substring(0, 16), serviceId packed)
                Assert.AreEqual(splitTripId "base" original "selection", splitTripId "base" packed "selection")
                Assert.AreEqual(addedSourceTripId "generic" "trip" original, addedSourceTripId "generic" "trip" packed)
                Assert.AreSame(packed, pool.Intern original)
                Assert.AreEqual(original |> Array.filter id |> Array.length, packed.Count)

    [<TestMethod>]
    member _.``External sort handles interleaved groups quoted fields and many spill runs``() =
        use storage = new Scratch.Storage(Path.GetTempPath())
        let input = [| for i in 100 .. -1 .. 0 -> [| string (i % 7); string i; "řádek,\"quoted\"\ncontinued" |] |]
        let compareRows (a: string array) (b: string array) = StringComparer.Ordinal.Compare(a.[0], b.[0])
        let actual = Scratch.sortRows storage compareRows 128L input |> Seq.toArray
        let expected = input |> Array.indexed |> Array.sortBy (fun (index, row) -> row.[0], index) |> Array.map snd
        // Equal keys preserve their original order across merge runs.
        CollectionAssert.AreEqual(expected |> Array.collect id, actual |> Array.collect id)
        Assert.AreEqual(0, Directory.GetFiles(storage.Directory).Length)
        Assert.AreEqual(0, Scratch.sortRows storage compareRows 128L Seq.empty |> Seq.length)

    [<TestMethod>]
    member _.``Spill failure removes all scratch files``() =
        let mutable directory = ""
        Assert.ThrowsExactly<IOException>(Action(fun () ->
            use storage = new Scratch.Storage(Path.GetTempPath())
            directory <- storage.Directory
            let input = seq {
                for i in 1 .. 40 do yield [| string i |]
                raise (IOException("Simulated input failure after spills"))
            }
            Scratch.sortRows storage (fun a b -> compare a.[0] b.[0]) 64L input |> Seq.length |> ignore)) |> ignore
        Assert.IsFalse(Directory.Exists(directory))

    [<TestMethod>]
    member _.``Indexed CSV and direct writer preserve multiline Unicode and empty fields``() =
        use storage = new Scratch.Storage(Path.GetTempPath())
        let input = [| [| "a,\"b\""; "žluťoučký\nřádek"; "" |]; [| ""; "x"; "last" |] |]
        let path = Path.Combine(storage.Directory, "table.txt")
        writeValues path [| "a"; "b"; "c" |] input
        let actual = csvValues storage.Directory "table.txt" [| "b"; "missing"; "a" |] |> Seq.toArray
        CollectionAssert.AreEqual([| input.[0].[1]; ""; input.[0].[0]; "x"; ""; "" |], actual |> Array.collect id)

    [<TestMethod>]
    member _.``Shape spool preserves canonical IDs with interleaved geometries``() =
        use storage = new Scratch.Storage(Path.GetTempPath())
        File.WriteAllText(Path.Combine(storage.Directory, "shapes.txt"),
            "shape_id,shape_pt_lat,shape_pt_lon,shape_pt_sequence,shape_dist_traveled\nb,50.0,14.00,2,1\na,49,13,1,0\nb,49.00,13,1,0\na,50,14,2,1\n", UTF8Encoding(false))
        let diagnostics = DiagnosticLog(storage)
        let shapes = Shapes.prepare storage (Shapes.spool storage storage.Directory) (Set.ofList ["a"; "b"]) diagnostics
        let expected = "overlay:shape:" + (sha256Text "49,13,0;50,14,1").Substring(0, 20)
        Assert.AreEqual(expected, shapes.outputBySource.["a"])
        Assert.AreEqual(expected, shapes.outputBySource.["b"])
        Assert.AreEqual(1, shapes.locations.Count)
        Assert.AreEqual(2, Shapes.rows shapes expected |> Seq.length)
        Assert.AreEqual(0, diagnostics.Rows |> Seq.length)

    [<TestMethod>]
    member _.``Unselected malformed shapes do not grant diagnostics or abort the overlay``() =
        use storage = new Scratch.Storage(Path.GetTempPath())
        File.WriteAllText(Path.Combine(storage.Directory, "shapes.txt"),
            "shape_id,shape_pt_lat,shape_pt_lon,shape_pt_sequence\nignored,bad,bad,not-a-sequence\nselected,49,13,1\nselected,50,14,2\n")
        let diagnostics = DiagnosticLog(storage)
        let shapes = Shapes.prepare storage (Shapes.spool storage storage.Directory) (Set.singleton "selected") diagnostics
        Assert.AreEqual(1, shapes.outputBySource.Count)
        Assert.AreEqual(0, diagnostics.Rows |> Seq.length)

    [<TestMethod>]
    member _.``Evidence flush failure leaves neither scratch nor activated output``() =
        let parent = Path.Combine(Path.GetTempPath(), "overlay-storage-failure-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(parent) |> ignore
        let output = Path.Combine(parent, "output")
        let mutable directory = ""
        try
            Assert.ThrowsExactly<IOException>(Action(fun () ->
                use storage = new Scratch.Storage(parent)
                directory <- storage.Directory
                let brokenStream = { new MemoryStream() with override _.Flush() = raise (IOException("Simulated disk failure")) }
                let writer = new BinaryWriter(brokenStream)
                storage.Own(writer)
                writer.Write("pending evidence")
                storage.Flush()
                Directory.CreateDirectory(output) |> ignore)) |> ignore
            Assert.IsFalse(Directory.Exists(directory))
            Assert.IsFalse(Directory.Exists(output))
        finally
            Directory.Delete(parent, true)

    [<TestMethod>]
    member _.``Winner selection preserves tier precedence equivalent ties and ambiguity``() =
        let candidate id tier delta : CandidateMatch = {
            tripId = id; tier = tier; sourceOrdinalByTarget = [| Some 0; Some 1 |]
            editCount = 0; alignedCallCount = 2; firstDepartureDelta = delta
            aggregateTimeDelta = int64 delta; durationDelta = 0; maximumTimeDelta = delta; squaredTimeDelta = int64 delta * int64 delta
        }
        let a, b = candidate "a" "exact" 10, candidate "b" "exact" 10
        let tierRank tier = if tier = "exact" then 0 else 1
        let choose = TripMatching.chooseCandidates tierRank
        match choose (fun _ -> "same") [| a; b; candidate "c" "fallback" 0 |] with
        | TripMatching.Accepted(winners, _) -> Assert.AreEqual(2, winners.Length)
        | other -> Assert.Fail($"Unexpected decision {other}")
        match choose (fun candidate -> candidate.tripId) [| a; b |] with
        | TripMatching.Ambiguous(winners, classes) -> Assert.AreEqual(2, classes); Assert.AreEqual(2, winners.Length)
        | other -> Assert.Fail($"Unexpected decision {other}")
        match choose (fun _ -> "same") [||] with
        | TripMatching.NoMatch -> ()
        | other -> Assert.Fail($"Unexpected decision {other}")
