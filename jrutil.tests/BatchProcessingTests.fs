// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
namespace JrUtil.Tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open JrUtil.Utils
open JrUtil.Tests.Asserts

[<TestClass>]
type BatchProcessingTests() =
    [<TestMethod>]
    member this.``Best effort batch action reports and skips failure``() =
        let mutable reported = false

        runBatchAction false (fun _ -> reported <- true) (fun () ->
            raise (InvalidOperationException("bad batch")))

        assertEqual true reported

    [<TestMethod>]
    member this.``Strict batch action reports and propagates failure``() =
        let mutable reported = false

        Assert.ThrowsExactly<InvalidOperationException>(fun () ->
            runBatchAction true (fun _ -> reported <- true) (fun () ->
                raise (InvalidOperationException("bad batch"))))
        |> ignore

        assertEqual true reported
