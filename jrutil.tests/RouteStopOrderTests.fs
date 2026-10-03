namespace JrUtil.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open JrUtil.Serving

[<TestClass>]
type RouteStopOrderTests() =
    // Locations are letters; times are fractions of the trip.
    let pattern (stops: string) weight (times: float list) : RouteStopOrder.Pattern =
        { locations = stops.ToCharArray() |> Array.map int
          weight = weight
          times = if times.IsEmpty then Array.create stops.Length Double.NaN else List.toArray times
          key = stops }

    let render (ordered: int array) = ordered |> Array.map char |> String

    [<TestMethod>]
    member _.``Versions with different stops merge into one ordered list``() =
        let ordered, assigned =
            RouteStopOrder.merge [|
                pattern "ABCD" 5 [ 0.0; 0.3; 0.5; 1.0 ]
                pattern "ABXD" 2 [ 0.0; 0.3; 0.6; 1.0 ] |]
        Assert.AreEqual("ABCXD", render ordered)
        CollectionAssert.AreEqual([| 0; 1; 2; 4 |], assigned.[0])
        CollectionAssert.AreEqual([| 0; 1; 3; 4 |], assigned.[1])

    [<TestMethod>]
    member _.``Inserted stops follow scheduled time within a gap``() =
        let ordered, _ =
            RouteStopOrder.merge [|
                pattern "ACD" 5 [ 0.0; 0.5; 1.0 ]
                pattern "ABD" 2 [ 0.0; 0.2; 1.0 ] |]
        Assert.AreEqual("ABCD", render ordered)

    [<TestMethod>]
    member _.``Short turns reuse the full line's slots``() =
        let ordered, assigned = RouteStopOrder.merge [| pattern "BC" 3 []; pattern "ABCD" 5 [] |]
        Assert.AreEqual("ABCD", render ordered)
        CollectionAssert.AreEqual([| 1; 2 |], assigned.[0])
        CollectionAssert.AreEqual([| 0; 1; 2; 3 |], assigned.[1])

    [<TestMethod>]
    member _.``Loop lines keep one slot per visit``() =
        let ordered, assigned = RouteStopOrder.merge [| pattern "ABCA" 4 []; pattern "ABC" 1 [] |]
        Assert.AreEqual("ABCA", render ordered)
        CollectionAssert.AreEqual([| 0; 1; 2; 3 |], assigned.[0])
        CollectionAssert.AreEqual([| 0; 1; 2 |], assigned.[1])

    [<TestMethod>]
    member _.``Merge does not depend on input order``() =
        let patterns = [|
            pattern "ABCDE" 3 [ 0.0; 0.2; 0.4; 0.7; 1.0 ]
            pattern "ABXDE" 3 [ 0.0; 0.2; 0.5; 0.7; 1.0 ]
            pattern "BCD" 2 []
            pattern "AYE" 1 [ 0.0; 0.9; 1.0 ] |]
        let expected, expectedAssigned = RouteStopOrder.merge patterns
        let random = Random(7)
        for _ in 1 .. 20 do
            let permutation = Array.init patterns.Length id |> Array.sortBy (fun _ -> random.Next())
            let ordered, assigned = RouteStopOrder.merge (permutation |> Array.map (fun index -> patterns.[index]))
            Assert.AreEqual(render expected, render ordered)
            permutation |> Array.iteri (fun position index ->
                CollectionAssert.AreEqual(expectedAssigned.[index], assigned.[position]))
