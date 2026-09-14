// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

namespace JrUtil.Tests

open System
open System.Collections.Generic
open System.IO
open System.IO.Compression
open System.Text.Json
open System.Threading
open Microsoft.VisualStudio.TestTools.UnitTesting
open OsmSharp
open OsmSharp.Streams
open OsmSharp.Tags
open Parquet
open Parquet.Serialization

open JrUtil
open JrUtil.Tests.Asserts

[<TestClass>]
type JdfBundleTests() =
    let fixturePath =
        Path.Combine(__SOURCE_DIRECTORY__, "data", "jdf", "obehy_extensions")

    let permissiveCoveragePolicy =
        let baseline=JdfPostInferencePolicy.conservativeRoutedV4
        { baseline with
            policyId="test-permissive-coverage"
            consolidation={baseline.consolidation with maximumDiameterMetres=25.0}
            resolution={baseline.resolution with
                            minimumPlausibleScore=0.45
                            minimumPhysicalScore=0.70
                            minimumPhysicalMargin=0.15
                            materialRoutedAdvantage=0.10}
            consensus={baseline.consensus with minimumContexts=3;minimumWinningShare=1.0}
            alternativeCorridors={baseline.alternativeCorridors with
                                      absoluteTieMetres=25.0;relativeTieFraction=0.05}
            sideGroups={baseline.sideGroups with
                            maximumCompactnessMetres=35.0
                            additionalGroupMinimumContexts=3} }

    let descriptor path kind sha bytes =
        File.WriteAllText(path,
            $$"""{
  "schema_version": 1,
  "source_id": "national-jdf",
  "retrieved_at": "2026-07-18T12:00:00+02:00",
  "retrieval_method": "fixture",
  "source_uri": null,
  "licence": "synthetic-test-data",
  "payload_kind": "{{kind}}",
  "payload_sha256": "{{sha}}",
  "payload_bytes": {{bytes}}
}""")

    let readParquet path =
        use stream = File.OpenRead(path)
        ParquetSerializer.DeserializeUntypedAsync(stream).GetAwaiter().GetResult()

    let assertSchema root fileName expected =
        let result = readParquet (Path.Combine(root, fileName))
        let actual =
            result.Schema.DataFields
            |> Array.map (fun field -> field.Name, field.ClrType, field.IsNullable)
        assertEqual expected actual
        result

    let osmTags pairs =
        let result=TagsCollection()
        for key,value in pairs do result.Add(key,value)
        result
    let osmNode id latitude longitude =
        Node(Id=Nullable id,Version=Nullable 1L,ChangeSetId=Nullable 0L,
             Visible=Nullable true,TimeStamp=Nullable DateTime.UnixEpoch,
             UserId=Nullable 0L,UserName="",Latitude=Nullable latitude,
             Longitude=Nullable longitude,Tags=osmTags [])
    let osmWay id nodes tags =
        Way(Id=Nullable id,Version=Nullable 1L,ChangeSetId=Nullable 0L,
            Visible=Nullable true,TimeStamp=Nullable DateTime.UnixEpoch,
            UserId=Nullable 0L,UserName="",Nodes=nodes,Tags=osmTags tags)
    let writeOsmPbf path (objects:OsmGeo array) =
        use stream=File.Create(path)
        let target=PBFOsmStreamTarget(stream,true,Nullable<int>(),Nullable<int>())
        target.RegisterSource(new OsmEnumerableStreamSource(objects))
        target.Pull()

    let routedCaptureFixture root =
        let source=Jdf.jdfBatchDirParser () (Jdf.FsPath fixturePath)
        let observation stopId id lat lon modes denied lifecycle:JdfModel.PostCandidateEvidence = {
            stopId=stopId;candidateId=id;observationId=id;sourceKind="test"
            sourceObjectId=Some id;observedAt=None;lat=lat;lon=lon
            supportWeight=1M;rawTags="";explicitModes=modes;deniedModes=denied
            lifecycle=lifecycle }
        let firstCall=
            source.tripStops |> Seq.toArray
            |> Array.find(fun value -> value.routeId="586001" && value.tripId=1L
                                       && value.routeStopId=1L)
        let thirdCall=
            source.tripStops |> Seq.toArray
            |> Array.find(fun value -> value.routeId="586001" && value.tripId=1L
                                       && value.routeStopId=3L)
        let routeStop3=
            source.routeStops
            |> Array.find(fun value -> value.routeId="586001" && value.routeStopId=3L)
        let tripStops=
            source.tripStops |> Seq.toArray
            |> Array.map(fun value ->
                if value.routeId="586001" && value.tripId=1L && value.routeStopId=3L then
                    {value with departureTime=firstCall.departureTime}
                else value)
            |> Array.append [|{thirdCall with routeStopId=4L
                                              departureTime=firstCall.departureTime}|]
            |> Array.sortBy(fun value -> value.routeId,value.routeDistinction,
                                         value.tripId,value.routeStopId)
        let routed={source with
                        routeStops=(
                            Array.append source.routeStops [|{routeStop3 with routeStopId=4L}|]
                            |> Array.sortBy(fun value ->
                                value.routeId,value.routeDistinction,value.routeStopId))
                        tripStops=tripStops
                        stopLocations=[|
                            {stopId=100L;lat=50.0M;lon=14.0M;precision=JdfModel.StopPrecise}
                            {stopId=200L;lat=50.01M;lon=14.0M;precision=JdfModel.StopPrecise}|]
                        postCandidateEvidence=[|
                            observation 100L "100-east" 50.0M 14.00005M "road" "" "active"
                            observation 100L "100-west" 50.0M 13.99995M "road" "" "active"
                            observation 100L "100-obsolete" 50.0M 14.00008M "road" "" "disused"
                            observation 100L "100-conflict" 50.00002M 14.00010M "tram" "road" "active"
                            observation 200L "200-east" 50.01M 14.00005M "road" "" "active"
                            observation 200L "200-west" 50.01M 13.99995M "road" "" "active"
                            observation 200L "200-far" 50.01M 14.02M "road" "" "active" |]}
        let input=Path.Combine(root,"input.zip")
        use archive=ZipFile.Open(input,ZipArchiveMode.Create)
        Jdf.jdfBatchDirWriter () (Jdf.ZipArchive archive) routed
        archive.Dispose()
        let routing=Path.Combine(root,"routing.osm.pbf")
        writeOsmPbf routing [|
            osmNode 1L 49.99 14.0 :> OsmGeo;osmNode 2L 50.00 14.0 :> OsmGeo
            osmNode 3L 50.01 14.0 :> OsmGeo;osmNode 4L 50.02 14.0 :> OsmGeo
            osmNode 11L 49.99 14.00012 :> OsmGeo;osmNode 12L 50.00 14.00012 :> OsmGeo
            osmNode 13L 50.01 14.00012 :> OsmGeo;osmNode 14L 50.02 14.00012 :> OsmGeo
            osmWay 10L [|1L;2L;3L;4L|] ["highway","residential"] :> OsmGeo
            osmWay 11L [|11L;12L;13L;14L|] ["highway","residential"] :> OsmGeo
            osmWay 12L [|1L;11L|] ["highway","residential"] :> OsmGeo
            osmWay 13L [|4L;14L|] ["highway","residential"] :> OsmGeo|]
        File.WriteAllText(routing+".manifest.json",JsonSerializer.Serialize(
            {|filter_schema=JdfBundle.routingEnvelopePolicy;source_key="fixture"
              output={|bytes=FileInfo(routing).Length|}|}))
        let sha=
            use stream=File.OpenRead(input)
            Security.Cryptography.SHA256.HashData(stream) |> Convert.ToHexString
            |> _.ToLowerInvariant()
        let descriptorPath=Path.Combine(root,"snapshot.json")
        descriptor descriptorPath "zip" sha (FileInfo(input).Length)
        descriptorPath,input,routing

    let rewriteEvidenceRows path
                            (transform:IDictionary<string,obj> array -> IDictionary<string,obj> array) =
        let result =
            use source=File.OpenRead(path)
            ParquetSerializer.DeserializeUntypedAsync(source).GetAwaiter().GetResult()
        let rows=result.Data |> Seq.map(fun row -> row :> IDictionary<string,obj>) |> Seq.toArray
        let rows=transform rows
        let metadata=Dictionary<string,string>(result.CustomMetadata)
        use target=File.Create(path)
        ParquetSerializer.SerializeUntypedAsync(
            rows :> IReadOnlyCollection<IDictionary<string,obj>>,result.Schema,target,
            ParquetOptions(CompressionMethod=CompressionMethod.Snappy),metadata,
            CancellationToken.None).GetAwaiter().GetResult()

    let rewriteEvidenceRelation path (mutate:IDictionary<string,obj> -> unit) =
        rewriteEvidenceRows path (fun rows -> mutate rows.[0];rows)

    let rehashEvidenceRelation evidence fileName =
        let path=Path.Combine(evidence,fileName)
        let hash =
            use stream=File.OpenRead(path)
            Security.Cryptography.SHA256.HashData(stream) |> Convert.ToHexString
            |> _.ToLowerInvariant()
        let manifest=JdfPostInference.loadEvidenceManifest evidence
        let files=manifest.files |> Array.map(fun value ->
            if value.path=fileName then {value with sha256=hash;bytes=FileInfo(path).Length}
            else value)
        JdfPostInference.writeEvidenceManifest (Path.Combine(evidence,"manifest.json"))
            {manifest with files=files}

    let rehashEvidenceRelationWithCount evidence fileName rowCount =
        rehashEvidenceRelation evidence fileName
        let manifest=JdfPostInference.loadEvidenceManifest evidence
        let files=manifest.files |> Array.map(fun value ->
            if value.path=fileName then {value with rows=rowCount} else value)
        let manifest =
            match fileName with
            | "observations.parquet" -> {manifest with files=files;observationCount=rowCount}
            | "route_points.parquet" -> {manifest with files=files;routePointCount=rowCount}
            | "contexts.parquet" -> {manifest with files=files;contextCount=rowCount}
            | "corridor_variants.parquet" -> {manifest with files=files;corridorVariantCount=rowCount}
            | "route_point_evidence.parquet" -> {manifest with files=files;routePointEvidenceCount=rowCount}
            | _ -> invalidArg "fileName" fileName
        JdfPostInference.writeEvidenceManifest (Path.Combine(evidence,"manifest.json")) manifest

    let copyDirectory source destination =
        Directory.CreateDirectory(destination) |> ignore
        for path in Directory.GetFiles(source) do
            File.Copy(path,Path.Combine(destination,Path.GetFileName(path)))

    [<TestMethod>]
    member _.``Mandatory routed review selectors and expectations stay in sync``() =
        let dataPath name = Path.Combine(__SOURCE_DIRECTORY__, "TestData", name)
        let selectors =
            File.ReadAllLines(dataPath "routed-post-review-stops.txt")
            |> Array.map _.Trim()
            |> Array.filter (fun value -> value <> "" && not(value.StartsWith("#")))
            |> Array.map Int64.Parse
            |> Set
        let expectationRows =
            File.ReadAllLines(dataPath "routed-post-review-expectations.tsv")
            |> Array.skip 1
            |> Array.filter (String.IsNullOrWhiteSpace >> not)
            |> Array.map (fun row -> row.Split('\t'))
        Assert.IsTrue(expectationRows |> Array.forall (fun row -> row.Length = 7))
        let expectedIds = expectationRows |> Array.map (fun row -> Int64.Parse(row.[0]))
        assertEqual expectedIds.Length (expectedIds |> Array.distinct |> Array.length)
        assertEqual selectors (expectedIds |> Set)
        let row stopId=expectationRows |> Array.find(fun values -> values.[0]=string stopId)
        assertEqual [|"2";"2";"false";"true"|] (row 3832 |> Array.skip 3)
        assertEqual [|"2";"2";"false";"false"|] (row 4410 |> Array.skip 3)

    [<TestMethod>]
    member _.``Routing PBF manifest accepts only the current envelope policy``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-routing-manifest-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let routingPbf = Path.Combine(root, "routing.osm.pbf")
            File.WriteAllBytes(routingPbf, [| 1uy; 2uy; 3uy |])
            let writeManifest policy =
                let manifest =
                    {| filter_schema = policy
                       source_key = "fixture-source"
                       output = {| bytes = 3 |} |}
                File.WriteAllText(
                    routingPbf + ".manifest.json",
                    JsonSerializer.Serialize(manifest))
            writeManifest JdfBundle.routingEnvelopePolicy
            JdfBundle.validateRoutingPbfManifest routingPbf
            writeManifest "jdf-routing-envelope-v1"
            Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfBundle.validateRoutingPbfManifest routingPbf)
            |> ignore
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``JDF bundle is deterministic and contains normalized sidecars``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-bundle-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let sha, bytes = JdfBundle.directoryTreeIdentity fixturePath
            let descriptorPath = Path.Combine(root, "snapshot.json")
            descriptor descriptorPath "directory-tree" sha bytes
            let first = Path.Combine(root, "first")
            let second = Path.Combine(root, "second")
            JdfBundle.writeBundle descriptorPath "test-commit" false fixturePath first
            JdfBundle.writeBundleWithPolicyAndMemory true descriptorPath "test-commit" false
                JdfToGtfs.KeepAll [||] fixturePath second

            JrUtil.Serving.Validation.validatePackage first |> ignore
            JrUtil.Serving.Validation.validatePackage second |> ignore
            JrUtil.Serving.Validation.compareByteIdentical first second

            let files = Directory.GetFiles(first, "*", SearchOption.AllDirectories)
            assertEqual 44 files.Length
            assertEqual false (Directory.Exists(Path.Combine(first, "gtfs-intermediate")))
            assertEqual false (File.Exists(Path.Combine(first, "source_call_metadata.parquet")))

            use manifest = JsonDocument.Parse(File.ReadAllBytes(Path.Combine(first, "manifest.json")))
            assertEqual "jrutil-production" (manifest.RootElement.GetProperty("bundle_format").GetString())
            assertEqual 1 (manifest.RootElement.GetProperty("bundle_version").GetInt32())
            assertEqual 2 (manifest.RootElement.GetProperty("serving_schema_version").GetInt32())
            assertEqual 37 (manifest.RootElement.GetProperty("relations").EnumerateArray() |> Seq.length)

            let relation name columns =
                JrUtil.Serving.PackageReader.readTextRows
                    (Path.Combine(first, "serving", name + ".parquet")) columns
                |> Seq.toArray
            assertEqual true ((relation "source_call_map" [|"binding_id"; "source_sequence"; "call_sequence"|]).Length > 0)
            assertEqual 5 ((relation "route_stop_zone" [|"route_id"; "route_stop_id"; "zone_id"|]).Length)
            assertEqual 3 ((relation "service_note" [|"note_id"; "kind"; "text"|]).Length)
            assertEqual 1 ((relation "connection_claim" [|"connection_id"; "origin_trip_id"; "wait_minutes"|]).Length)
            assertEqual 5 ((relation "travel_restriction_assignment" [|"assignment_id"; "scope"; "group_code"|]).Length)

            let scratch = Path.Combine(root, "compiler-view")
            let gtfsPath, _ = JrUtil.Serving.PackageReader.prepareCompilerView first scratch
            let parsed = Gtfs.gtfsParseFolder () gtfsPath
            parsed.trips |> Seq.iter (fun trip ->
                assertEqual true (trip.serviceId.StartsWith("gtfs:service:")))
            let callKeys = parsed.stopTimes |> Seq.map (fun call -> call.tripId, call.stopSequence) |> set
            assertEqual true (callKeys.Count > 0)
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Streamed call metadata retains ordinal trip ordering and GTFS sequences``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-bundle-call-order-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let source = Jdf.jdfBatchDirParser () (Jdf.FsPath fixturePath)
            let templateTrip =
                source.trips
                |> Array.find (fun trip -> trip.routeId = "586001" && trip.id = 1L)
            let copiedCalls =
                source.tripStops |> Seq.toArray
                |> Array.filter (fun call ->
                    call.routeId = templateTrip.routeId
                    && call.routeDistinction = templateTrip.routeDistinction
                    && call.tripId = templateTrip.id)
                |> Array.map (fun call -> { call with tripId = 11L })
            let modified = {
                source with
                    trips = Array.append source.trips [| { templateTrip with id = 11L } |]
                    tripStops = Array.append (source.tripStops |> Seq.toArray) copiedCalls
            }
            let input = Path.Combine(root, "jdf")
            Jdf.jdfBatchDirWriter () (Jdf.FsPath input) modified
            let sha, bytes = JdfBundle.directoryTreeIdentity input
            let descriptorPath = Path.Combine(root, "snapshot.json")
            descriptor descriptorPath "directory-tree" sha bytes
            let output = Path.Combine(root, "bundle")
            JdfBundle.writeBundle descriptorPath "test-commit" false input output

            let bindings =
                JrUtil.Serving.PackageReader.readTextRows
                    (Path.Combine(output, "serving", "source_trip_map.parquet"))
                    [|"binding_id"; "trip_id"|]
                |> Seq.toArray
            let tripIds = bindings |> Array.map (fun row -> row.[1])
            let copiedTripId = "jdf:trip:586001:1:11"
            assertEqual true (tripIds |> Array.contains copiedTripId)

            let scratch = Path.Combine(root, "compiler-view")
            let gtfsPath, _ = JrUtil.Serving.PackageReader.prepareCompilerView output scratch
            let gtfs = Gtfs.gtfsParseFolder () gtfsPath
            let expectedSequences =
                gtfs.stopTimes
                |> Seq.filter (fun call -> call.tripId = copiedTripId)
                |> Seq.map (fun call -> call.stopSequence)
                |> Seq.toArray
            let bindingIds =
                bindings
                |> Seq.filter (fun row -> row.[1] = copiedTripId)
                |> Seq.map (fun row -> row.[0])
                |> set
            let actualSequences =
                JrUtil.Serving.PackageReader.readTextRows
                    (Path.Combine(output, "serving", "source_call_map.parquet"))
                    [|"binding_id"; "call_sequence"|]
                |> Seq.filter (fun row -> bindingIds.Contains(row.[0]))
                |> Seq.map (fun row -> Convert.ToInt32(row.[1]))
                |> Seq.toArray
            assertEqual expectedSequences actualSequences
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``JDF bundle validates snapshots and ZIP packaging atomically``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-bundle-errors-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let descriptorPath = Path.Combine(root, "snapshot.json")
            let output = Path.Combine(root, "invalid")
            descriptor descriptorPath "directory-tree" (String.replicate 64 "0") 0L
            Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfBundle.writeBundle descriptorPath "test-commit" false fixturePath output)
            |> ignore
            assertEqual false (Directory.Exists(output))

            let zipPath = Path.Combine(root, "fixture.zip")
            ZipFile.CreateFromDirectory(fixturePath, zipPath)
            let zipSha =
                use stream = File.OpenRead(zipPath)
                Security.Cryptography.SHA256.HashData(stream)
                |> Convert.ToHexString
                |> fun value -> value.ToLowerInvariant()
            descriptor descriptorPath "zip" zipSha (FileInfo(zipPath).Length)
            let zipOutput = Path.Combine(root, "zip")
            JdfBundle.writeBundle descriptorPath "test-commit" true zipPath zipOutput
            assertEqual true (File.Exists(Path.Combine(zipOutput, "manifest.json")))

            let unsafeZip = Path.Combine(root, "unsafe.zip")
            use unsafeArchive = ZipFile.Open(unsafeZip, ZipArchiveMode.Create)
            unsafeArchive.CreateEntry("../VerzeJDF.txt") |> ignore
            unsafeArchive.Dispose()
            let unsafeSha =
                use stream = File.OpenRead(unsafeZip)
                Security.Cryptography.SHA256.HashData(stream)
                |> Convert.ToHexString
                |> fun value -> value.ToLowerInvariant()
            descriptor descriptorPath "zip" unsafeSha (FileInfo(unsafeZip).Length)
            let unsafeOutput = Path.Combine(root, "unsafe")
            Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfBundle.writeBundle descriptorPath "test-commit" false unsafeZip unsafeOutput)
            |> ignore
            assertEqual false (Directory.Exists(unsafeOutput))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Routed bundle rejects candidate-free JDF before opening routing graph``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-bundle-no-post-candidates-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let sha, bytes = JdfBundle.directoryTreeIdentity fixturePath
            let descriptorPath = Path.Combine(root, "snapshot.json")
            descriptor descriptorPath "directory-tree" sha bytes
            let output = Path.Combine(root, "bundle")
            let missingRoutingPbf = Path.Combine(root, "missing-routing.osm.pbf")
            let error =
                Assert.ThrowsExactly<ArgumentException>(fun () ->
                    JdfBundle.writeBundleWithRoutedPostInference
                        descriptorPath "test-commit" false
                        JdfToGtfs.KeepAll [||] JdfToGtfs.emptyTransportModeRules
                        true (Some missingRoutingPbf) false fixturePath output)
            StringAssert.Contains(error.Message, "JrutilPostCandidateEvidence.txt")
            assertEqual false (Directory.Exists(output))
        finally
            if Directory.Exists(root) then Directory.Delete(root, true)

    [<TestMethod>]
    member _.``Capture-only dispatch never enters policy evaluation or GTFS publication``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-capture-only-isolation-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let descriptorPath,input,routing=routedCaptureFixture root
            let evidence=Path.Combine(root,"evidence")
            let phases=ResizeArray<string>()
            JdfPostInferenceEvaluator.resetEvaluatorEntryCount()
            JdfPostInferencePolicy.PostInferencePhaseProbe.reset()
            let options={JdfBundle.defaultBundleExecutionOptions with
                            maximumWorkers=3;memoryBudgetBytes=1L
                            postInferenceEvidenceOnly=true
                            capturePostInferenceEvidencePath=Some evidence
                            progress=fun value -> phases.Add(value.phase)}
            let result=JdfBundle.executeBundleWithRoutedPostInferenceOptions
                           descriptorPath "test-tool" false JdfToGtfs.KeepAll [||]
                           JdfToGtfs.emptyTransportModeRules true (Some routing) false
                           options input (Path.Combine(root,"must-not-be-created"))
            match result with
            | JdfBundle.CaptureCompleted(manifest,metrics) ->
                Assert.IsTrue(manifest.routePointEvidenceCount>0L)
                Assert.IsTrue(metrics.estimatedEvidenceBytes>0L)
                assertEqual
                    (metrics.estimatedEvidenceBytes+JdfBundle.PostEvidenceOutputSafetyReserveBytes)
                    metrics.atomicOutputHeadroomBytes
                Assert.IsTrue(metrics.peakSpillBytes>=metrics.currentSpillBytes)
                assertEqual 3 metrics.maximumWorkers
            | _ -> Assert.Fail("Capture-only execution did not return CaptureCompleted")
            assertEqual 0L (JdfPostInferenceEvaluator.evaluatorEntryCount())
            let probes=JdfPostInferencePolicy.PostInferencePhaseProbe.snapshot()
            let forbiddenProbes=[|"policy-loading";"evaluator-entry";"consolidation";"scoring"
                                  "resolution";"authored-selection";"result-adaptation";"gtfs-conversion"
                                  "bundle-staging";"prepare-inference";"stream-stop-times"
                                  "prepare-remaining-gtfs";"write-relations";"write-diagnostics"
                                  "hash-payloads";"activation"|]
            for phase in forbiddenProbes do
                assertEqual 0L (probes |> Map.tryFind phase |> Option.defaultValue 0L)
            Assert.IsTrue(File.Exists(Path.Combine(evidence,"manifest.json")))
            Assert.IsFalse(Directory.Exists(Path.Combine(root,"must-not-be-created")))
            let forbidden=[|"evaluate-post-inference";"replay-post-inference";"prepare-inference"
                            "write-derived";"stage-bundle";"activate";"diagnostics"|]
            for phase in forbidden do
                Assert.IsFalse(phases |> Seq.exists(fun value -> value.Contains(phase)),
                               $"Capture-only unexpectedly entered {phase}")
            let allowedPrefixes=[|"validate-snapshot";"parse-jdf";"prepare-calendar";"calendar-trips"
                                  "filter-international";"capture-disk-preflight";"prepare-routing-snaps"
                                  "graph-";"routing-";"capture-routing-";"capture-post-inference-evidence"
                                  "capture-evidence-"|]
            for phase in phases |> Seq.distinct do
                Assert.IsTrue(allowedPrefixes |> Array.exists phase.StartsWith,
                              $"Unexpected capture-only phase: {phase}")
            Assert.IsTrue(phases |> Seq.exists(fun value -> value.StartsWith("routing-")))
        finally
            if Directory.Exists(root) then Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Capture preflight counts deduplicated route patterns rather than timetable calls``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-capture-preflight-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let _,input,routing=routedCaptureFixture root
            use archive=ZipFile.OpenRead(input)
            let batch=Jdf.jdfBatchDirParser () (Jdf.ZipArchive archive)
            let repeatedCalls =
                [| for copy in 0..499 do
                       for call in batch.tripStops do
                           let copied={call with tripId=call.tripId+int64 copy*1_000_000L}
                           if call.stopId=200L then
                               yield {copied with stopPostId=None
                                                  stopPostNum=Some $"remote-{copy}"}
                           else yield copied |]
                |> Array.sortBy(fun call -> call.routeId,call.routeDistinction,call.tripId,call.routeStopId)
            let repeated={batch with tripStops=repeatedCalls}
            use graph=JrUtil.GeoData.Osm.PackedRoutingGraph.Open(routing)
            let captureBounds value =
                let mutable bounds=None
                let mutable routed=0L
                let mutable derivedSpill = -1L
                let options =
                    {JdfPostEvidence.defaultCaptureOptions with
                        maximumWorkers=2;memoryBudgetBytes=1L
                        preflight=fun value -> bounds<-Some value
                        progress=fun phase count _ _ ->
                            if phase="capture-routing-contexts" then routed<-max routed count}
                use captured=JdfPostEvidence.captureToStore options graph value
                derivedSpill<-
                    captured.contexts.CurrentSpillBytes+
                    captured.corridorVariants.CurrentSpillBytes+
                    captured.routePointEvidence.CurrentSpillBytes
                bounds.Value,routed,derivedSpill
            let baseline,_,_=captureBounds batch
            let expanded,routed,derivedSpill=captureBounds repeated
            Assert.IsTrue(expanded.sourceContextCount>expanded.contextCount)
            Assert.IsTrue(expanded.contextCount<int64 repeated.tripStops.Count)
            Assert.IsTrue(expanded.routingEvidenceCount<expanded.contextCount)
            Assert.IsTrue(expanded.routingEvidenceCount>=baseline.routingEvidenceCount)
            assertEqual expanded.routingEvidenceCount routed
            assertEqual 0L derivedSpill
            let estimated=JdfBundle.estimatePostEvidenceOutputBytes expanded
            let expected =
                expanded.observationCount*384L
                + expanded.routePointCount*320L
                + expanded.contextCount*1024L
                + expanded.corridorVariantCount*512L
                + expanded.routePointEvidenceCount*384L
            assertEqual expected estimated
        finally
            if Directory.Exists(root) then Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Post evidence excludes candidates outside the precise parent centroid radius``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-post-candidate-radius-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let _,input,routing=routedCaptureFixture root
            use archive=ZipFile.OpenRead(input)
            let source=Jdf.jdfBatchDirParser () (Jdf.ZipArchive archive)
            let observation id latitude : JdfModel.PostCandidateEvidence = {
                stopId=100L;candidateId=id;observationId=id;sourceKind="test"
                sourceObjectId=Some id;observedAt=None;lat=latitude;lon=14.0M
                supportWeight=1M;rawTags="";explicitModes="road";deniedModes="";lifecycle="active" }
            let inside=observation "100-inside-300m" (50.0M+299.999M/110540M)
            let outside=observation "100-outside-300m" (50.0M+300.001M/110540M)
            let withoutPreciseCentroid =
                { source with
                    stopLocations=source.stopLocations |> Array.map(fun location ->
                        if location.stopId=200L then {location with precision=JdfModel.Estimated} else location)
                    postCandidateEvidence=Array.append source.postCandidateEvidence [|inside;outside|] }
            use graph=JrUtil.GeoData.Osm.PackedRoutingGraph.Open(routing)
            let options:JdfPostEvidence.PostEvidenceCaptureOptions = {
                maximumWorkers=2;memoryBudgetBytes=Int64.MaxValue;preflight=ignore
                progress=fun _ _ _ _ -> () }
            use captured=JdfPostEvidence.captureToStore options graph withoutPreciseCentroid
            let observations=captured.observations.ReadRows() |> Seq.toArray
            let routePoints=captured.routePoints.ReadRows() |> Seq.toArray
            Assert.IsTrue(observations |> Array.exists(fun value -> value.observationId=inside.observationId))
            Assert.IsFalse(observations |> Array.exists(fun value -> value.observationId=outside.observationId))
            Assert.IsFalse(observations |> Array.exists(fun value -> value.observationId="200-far"))
            Assert.IsFalse(observations |> Array.exists(fun value -> value.stopId=200L))
            Assert.IsFalse(routePoints |> Array.exists(fun value -> value.stopId=200L))
        finally
            if Directory.Exists(root) then Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Capture cancellation and output collision leave no worker spools``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-capture-cleanup-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        let spoolRoot=Path.Combine(Path.GetTempPath(),"jrutil-post-evidence-capture")
        let spoolFiles () =
            if Directory.Exists(spoolRoot) then Directory.GetFiles(spoolRoot) |> Set.ofArray
            else Set.empty
        let before=spoolFiles()
        try
            let descriptorPath,input,routing=routedCaptureFixture root
            use archive=ZipFile.OpenRead(input)
            let batch=Jdf.jdfBatchDirParser () (Jdf.ZipArchive archive)
            use graph=JrUtil.GeoData.Osm.PackedRoutingGraph.Open(routing)
            use cancellation=new CancellationTokenSource()
            cancellation.Cancel()
            Assert.ThrowsExactly<OperationCanceledException>(fun () ->
                JdfPostEvidence.captureToStoreWithCancellation cancellation.Token
                    {maximumWorkers=4;memoryBudgetBytes=1L;preflight=ignore
                     progress=fun _ _ _ _ -> ()}
                    graph batch |> ignore)
            |> ignore
            assertEqual before (spoolFiles())

            let forcedSpill={JdfPostEvidence.defaultCaptureOptions with
                                maximumWorkers=4;memoryBudgetBytes=1L}
            let duplicateObservation=
                {batch with postCandidateEvidence=
                                Array.append batch.postCandidateEvidence
                                    [|batch.postCandidateEvidence.[0]|]}
            Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfPostEvidence.captureToStore forcedSpill graph duplicateObservation
                |> ignore)
            |> ignore
            assertEqual before (spoolFiles())

            let failedGraph=JrUtil.GeoData.Osm.PackedRoutingGraph.Open(routing)
            (failedGraph :> IDisposable).Dispose()
            let mutable routingFailed=false
            try JdfPostEvidence.captureToStore forcedSpill failedGraph batch |> ignore
            with _ -> routingFailed<-true
            Assert.IsTrue(routingFailed,"Disposed routing graph should fail capture")
            assertEqual before (spoolFiles())

            let collision=Path.Combine(root,"collision")
            Directory.CreateDirectory(collision) |> ignore
            let sentinel=Path.Combine(collision,"owned.txt")
            File.WriteAllText(sentinel,"preserve")
            let options={JdfBundle.defaultBundleExecutionOptions with
                            maximumWorkers=4;memoryBudgetBytes=1L
                            postInferenceEvidenceOnly=true
                            capturePostInferenceEvidencePath=Some collision}
            Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfBundle.executeBundleWithRoutedPostInferenceOptions
                    descriptorPath "test-tool" false JdfToGtfs.KeepAll [||]
                    JdfToGtfs.emptyTransportModeRules true (Some routing) false
                    options input (Path.Combine(root,"unused")) |> ignore)
            |> ignore
            assertEqual "preserve" (File.ReadAllText(sentinel))
            Assert.IsFalse(Directory.GetDirectories(root,".collision.tmp-*" ).Length>0)
            assertEqual before (spoolFiles())
        finally
            if Directory.Exists(root) then Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Published capture bytes ignore input order workers and spill chunks``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-capture-bytes-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let _,input,routing=routedCaptureFixture root
            use archive=ZipFile.OpenRead(input)
            let batch=Jdf.jdfBatchDirParser () (Jdf.ZipArchive archive)
            let reversed={batch with
                            tripStops=Array.rev (batch.tripStops |> Seq.toArray)
                            postCandidateEvidence=Array.rev batch.postCandidateEvidence}
            let inputSha =
                use stream=File.OpenRead(input)
                Security.Cryptography.SHA256.HashData(stream) |> Convert.ToHexString
                |> _.ToLowerInvariant()
            let snapshot:JdfBundle.SnapshotDescriptor = {
                sourceId="national-jdf";retrievedAt="2026-07-18T12:00:00+02:00"
                retrievalMethod="fixture";sourceUri=None;licence="synthetic-test-data"
                payloadKind="zip";payloadSha256=inputSha;payloadBytes=FileInfo(input).Length }
            use graph=JrUtil.GeoData.Osm.PackedRoutingGraph.Open(routing)
            let variants=[|
                "one-worker-memory",1,Int64.MaxValue,batch
                "many-workers-memory",4,Int64.MaxValue,batch
                "reversed-input",4,Int64.MaxValue,reversed
                "forced-spill",4,1L,reversed |]
            for name,workers,budget,value in variants do
                let captureOptions:JdfPostEvidence.PostEvidenceCaptureOptions = {
                    maximumWorkers=workers;memoryBudgetBytes=budget
                    preflight=ignore
                    progress=fun _ _ _ _ -> () }
                use captured=JdfPostEvidence.captureToStore captureOptions graph value
                JdfBundle.writePostEvidenceStore snapshot "test-tool" false
                    (Path.Combine(root,name)) routing captured (fun _ _ _ -> ())
            let baseline=Path.Combine(root,"one-worker-memory")
            let relativeFiles=Array.append JdfPostInference.RequiredEvidenceFiles [|"manifest.json"|]
            for name,_,_,_ in variants |> Array.skip 1 do
                for relative in relativeFiles do
                    CollectionAssert.AreEqual(
                        File.ReadAllBytes(Path.Combine(baseline,relative)),
                        File.ReadAllBytes(Path.Combine(root,name,relative)),
                        name+":"+relative)
            for fileName in JdfPostInference.RequiredEvidenceFiles do
                let parquet=readParquet(Path.Combine(baseline,fileName))
                for value in parquet.CustomMetadata.Values do
                    Assert.IsFalse(value.Contains(root,StringComparison.OrdinalIgnoreCase))
                    Assert.IsFalse(value.Contains("worker",StringComparison.OrdinalIgnoreCase))
            let manifestText=File.ReadAllText(Path.Combine(baseline,"manifest.json"))
            Assert.IsFalse(manifestText.Contains(root,StringComparison.OrdinalIgnoreCase))
            Assert.IsFalse(manifestText.Contains("worker",StringComparison.OrdinalIgnoreCase))
        finally
            if Directory.Exists(root) then Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Live and replay share complete inference results and publication bytes``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-live-replay-equality-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let descriptorPath,input,routing=routedCaptureFixture root
            let evidence=Path.Combine(root,"evidence")
            let captureOptions={JdfBundle.defaultBundleExecutionOptions with
                                    maximumWorkers=3;memoryBudgetBytes=1L
                                    postInferenceEvidenceOnly=true
                                    capturePostInferenceEvidencePath=Some evidence}
            JdfBundle.executeBundleWithRoutedPostInferenceOptions
                descriptorPath "test-tool" false JdfToGtfs.KeepAll [||]
                JdfToGtfs.emptyTransportModeRules true (Some routing) false
                captureOptions input (Path.Combine(root,"capture-unused")) |> ignore
            use store=JdfPostEvidenceStore.openValidatedStore
                          JdfPostEvidenceStore.noIdentityExpectation evidence
            let capturedStops=store.ReadStops() |> Seq.toArray
            Assert.IsTrue(capturedStops.Length>1)
            let selectedStopsPath=Path.Combine(root,"selected-review-stops.txt")
            File.WriteAllText(selectedStopsPath,string capturedStops.[0]+Environment.NewLine)
            let selectedReplayReport=Path.Combine(root,"selected-replay-report")
            JdfBundle.replayPostInferenceEvidence evidence None None None
                (Some selectedStopsPath) selectedReplayReport
            Assert.IsTrue(File.Exists(Path.Combine(selectedReplayReport,"summary.json")))
            use first=JdfPostInferenceEvaluator.evaluate store permissiveCoveragePolicy
            use second=JdfPostInferenceEvaluator.evaluate store permissiveCoveragePolicy
            JdfPostInferencePolicy.PostInferencePhaseProbe.reset()
            use publicationOnly=
                JdfPostInferenceEvaluator.evaluateWithDiagnostics false store
                    permissiveCoveragePolicy
            let publicationProbes=JdfPostInferencePolicy.PostInferencePhaseProbe.snapshot()
            let firstAssignments=first.Assignments.ReadRows() |> Seq.toArray
            let firstDiagnostics=first.DiagnosticScores.ReadRows() |> Seq.toArray
            Assert.IsTrue(first.Hypotheses |> Array.exists(fun value ->
                value.memberObservationIds |> Array.contains "100-obsolete"))
            Assert.IsTrue(firstAssignments |> Array.exists(fun value -> value.assignmentKind="authored"))
            Assert.IsTrue(firstAssignments |> Array.exists(fun value -> value.sameStopBlockId.IsSome))
            Assert.IsTrue(first.Counters.sameStopBlocks>0)
            Assert.IsTrue(first.Counters.centroidResolutions>0)
            Assert.IsTrue(first.Counters.physicalResolutions>0 || first.Counters.sideResolutions>0)
            Assert.IsTrue(firstDiagnostics |> Array.exists(fun value -> value.alternativeCorridorCount>1))
            Assert.IsTrue(firstDiagnostics |> Array.exists(fun value -> value.modalityAdjustment<>0.0))
            Assert.IsTrue(firstDiagnostics |> Array.exists(fun value -> value.routingAvailability<>"available"))
            Assert.IsTrue(firstDiagnostics |> Array.exists(fun value ->
                value.signedLateralOffset |> Option.exists(fun offset -> offset<0.0)))
            Assert.IsTrue(firstDiagnostics |> Array.exists(fun value ->
                value.signedLateralOffset |> Option.exists(fun offset -> offset>0.0)))
            assertEqual first.Hypotheses second.Hypotheses
            assertEqual first.SideGroups second.SideGroups
            assertEqual first.AuthoredPositions second.AuthoredPositions
            assertEqual first.Counters second.Counters
            assertEqual first.Hypotheses publicationOnly.Hypotheses
            assertEqual first.SideGroups publicationOnly.SideGroups
            assertEqual first.AuthoredPositions publicationOnly.AuthoredPositions
            assertEqual first.Counters publicationOnly.Counters
            assertEqual firstAssignments
                        (second.Assignments.ReadRows() |> Seq.toArray)
            assertEqual firstAssignments
                        (publicationOnly.Assignments.ReadRows() |> Seq.toArray)
            Assert.AreEqual(0L,publicationOnly.DiagnosticScores.Count)
            assertEqual 1L
                        (publicationProbes |> Map.tryFind "evidence-joined-traversal"
                         |> Option.defaultValue 0L)
            assertEqual firstDiagnostics
                        (second.DiagnosticScores.ReadRows() |> Seq.toArray)

            let liveOutput=Path.Combine(root,"live")
            let replayOutput=Path.Combine(root,"replay")
            let liveOptions={JdfBundle.defaultBundleExecutionOptions with
                                maximumWorkers=3;memoryBudgetBytes=1L}
            JdfBundle.executeBundleWithRoutedPostInferenceOptions
                descriptorPath "test-tool" false JdfToGtfs.KeepAll [||]
                JdfToGtfs.emptyTransportModeRules true (Some routing) false
                liveOptions input liveOutput |> ignore
            let replayOptions={JdfBundle.defaultBundleExecutionOptions with
                                  memoryBudgetBytes=1L
                                  postInferenceEvidencePath=Some evidence}
            JdfBundle.executeBundleWithRoutedPostInferenceOptions
                descriptorPath "test-tool" false JdfToGtfs.KeepAll [||]
                JdfToGtfs.emptyTransportModeRules true None false
                replayOptions input replayOutput |> ignore
            JrUtil.Serving.Validation.validatePackage liveOutput |> ignore
            JrUtil.Serving.Validation.validatePackage replayOutput |> ignore
            CollectionAssert.AreEqual(
                File.ReadAllBytes(Path.Combine(liveOutput,"gtfs.zip")),
                File.ReadAllBytes(Path.Combine(replayOutput,"gtfs.zip")),"gtfs.zip")
            let liveView=Path.Combine(root,"live-view")
            let replayView=Path.Combine(root,"replay-view")
            let liveGtfs, _ = JrUtil.Serving.PackageReader.prepareCompilerView liveOutput liveView
            let replayGtfs, _ = JrUtil.Serving.PackageReader.prepareCompilerView replayOutput replayView
            let liveFeed=Gtfs.gtfsParseFolder () liveGtfs
            let replayFeed=Gtfs.gtfsParseFolder () replayGtfs
            let stopShape (feed:GtfsModel.GtfsFeed) =
                feed.stops |> Array.map(fun value -> value.id,value.parentStation) |> Array.sort
            assertEqual (stopShape liveFeed) (stopShape replayFeed)
            let unspecified (feed:GtfsModel.GtfsFeed) =
                feed.stopTimes |> Array.filter(fun value -> value.stopId.EndsWith(":unspecified"))
                               |> Array.map(fun value -> value.tripId,value.stopSequence,value.stopId)
                               |> Array.sort
            assertEqual (unspecified liveFeed) (unspecified replayFeed)
        finally
            if Directory.Exists(root) then Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Hash-consistent semantic corruption is rejected before evaluator entry``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-semantic-corruption-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let descriptorPath,input,routing=routedCaptureFixture root
            let evidence=Path.Combine(root,"evidence")
            let options={JdfBundle.defaultBundleExecutionOptions with
                            postInferenceEvidenceOnly=true
                            capturePostInferenceEvidencePath=Some evidence}
            JdfBundle.executeBundleWithRoutedPostInferenceOptions
                descriptorPath "test-tool" false JdfToGtfs.KeepAll [||]
                JdfToGtfs.emptyTransportModeRules true (Some routing) false
                options input (Path.Combine(root,"unused")) |> ignore
            rewriteEvidenceRelation (Path.Combine(evidence,"contexts.parquet"))
                (fun row -> row.["movement_family_id"] <- box "movement-family:corrupt")
            rehashEvidenceRelation evidence "contexts.parquet"
            JdfPostInferenceEvaluator.resetEvaluatorEntryCount()
            let error=Assert.ThrowsExactly<ArgumentException>(fun () ->
                use store=JdfPostEvidenceStore.openValidatedStore
                              JdfPostEvidenceStore.noIdentityExpectation evidence
                JdfPostInferenceEvaluator.evaluate store JdfPostInferencePolicy.conservativeRoutedV4
                |> ignore)
            StringAssert.Contains(error.Message,"movement-family")
            assertEqual 0L (JdfPostInferenceEvaluator.evaluatorEntryCount())
        finally
            if Directory.Exists(root) then Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Malformed evidence matrix is rejected before evaluator entry``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-malformed-evidence-matrix-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let descriptorPath,input,routing=routedCaptureFixture root
            let baseline=Path.Combine(root,"baseline")
            let options={JdfBundle.defaultBundleExecutionOptions with
                            postInferenceEvidenceOnly=true
                            capturePostInferenceEvidencePath=Some baseline}
            JdfBundle.executeBundleWithRoutedPostInferenceOptions
                descriptorPath "test-tool" false JdfToGtfs.KeepAll [||]
                JdfToGtfs.emptyTransportModeRules true (Some routing) false
                options input (Path.Combine(root,"unused")) |> ignore

            let semanticCases:(string*string*(IDictionary<string,obj> -> unit)) array=[|
                "observation-route-point-fk","observations.parquet",
                    fun row -> row.["route_point_id"]<-box "route-point:missing"
                "observation-coordinate-range","observations.parquet",
                    fun row -> row.["latitude"]<-box 100.0
                "route-point-representative-fk","route_points.parquet",
                    fun row -> row.["representative_observation_id"]<-box "observation:missing"
                "route-point-observation-membership","route_points.parquet",
                    fun row -> row.["observation_ids"]<-box "observation:missing"
                "context-canonical-id","contexts.parquet",
                    fun row -> row.["context_id"]<-box "context:corrupt"
                "context-authored-combination","contexts.parquet",
                    fun row -> row.["authored_post_key"]<-box "id:unexpected"
                "context-block-combination","contexts.parquet",
                    fun row -> row.["same_stop_block_role"]<-box "first";row.["same_stop_block_id"]<-null
                "context-movement-family","contexts.parquet",
                    fun row -> row.["movement_family_id"]<-box "movement-family:corrupt"
                "corridor-rank","corridor_variants.parquet",
                    fun row -> row.["variant_rank"]<-box 1
                "corridor-relative-cost","corridor_variants.parquet",
                    fun row -> row.["relative_cost_metres"]<-box -1.0
                "corridor-unavailable-sentinel","corridor_variants.parquet",
                    fun row -> row.["routing_availability"]<-box "unavailable";row.["absolute_cost_metres"]<-box 1.0
                "attachment-context-fk","route_point_evidence.parquet",
                    fun row -> row.["context_id"]<-box "context:missing"
                "attachment-route-point-fk","route_point_evidence.parquet",
                    fun row -> row.["route_point_id"]<-box "route-point:missing"
                "attachment-snap-range","route_point_evidence.parquet",
                    fun row -> row.["snap_fraction"]<-box 2.0
            |]
            let assertRejected name evidence =
                JdfPostInferenceEvaluator.resetEvaluatorEntryCount()
                Assert.ThrowsExactly<ArgumentException>(fun () ->
                    use store=JdfPostEvidenceStore.openValidatedStore
                                  JdfPostEvidenceStore.noIdentityExpectation evidence
                    JdfPostInferenceEvaluator.evaluate store JdfPostInferencePolicy.conservativeRoutedV4
                    |> ignore)
                |> ignore
                assertEqual 0L (JdfPostInferenceEvaluator.evaluatorEntryCount())
            for name,fileName,mutate in semanticCases do
                let evidence=Path.Combine(root,name)
                copyDirectory baseline evidence
                rewriteEvidenceRelation (Path.Combine(evidence,fileName)) mutate
                rehashEvidenceRelation evidence fileName
                assertRejected name evidence

            let structuralCases:(string*(IDictionary<string,obj> array -> IDictionary<string,obj> array)) array=[|
                "attachment-order",Array.rev
                "attachment-duplicate",fun rows -> Array.append rows [|rows.[0]|]
                "attachment-coverage",fun rows -> rows |> Array.tail
            |]
            for name,transform in structuralCases do
                let evidence=Path.Combine(root,name)
                copyDirectory baseline evidence
                let path=Path.Combine(evidence,"route_point_evidence.parquet")
                rewriteEvidenceRows path transform
                let rowCount =
                    use source=File.OpenRead(path)
                    ParquetSerializer.DeserializeUntypedAsync(source).GetAwaiter().GetResult().Data.Count
                    |> int64
                rehashEvidenceRelationWithCount evidence "route_point_evidence.parquet" rowCount
                assertRejected name evidence
        finally
            if Directory.Exists(root) then Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Evidence-backed bundle rejects a different merged JDF before replay``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-bundle-evidence-identity-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let sha, bytes = JdfBundle.directoryTreeIdentity fixturePath
            let descriptorPath = Path.Combine(root, "snapshot.json")
            descriptor descriptorPath "directory-tree" sha bytes
            let evidencePath=Path.Combine(root,"evidence")
            Directory.CreateDirectory(evidencePath) |> ignore
            let evidenceFiles=JdfPostInference.RequiredEvidenceFiles
            for fileName in evidenceFiles do File.WriteAllBytes(Path.Combine(evidencePath,fileName),[||])
            let emptyHash="e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
            JdfPostInference.writeEvidenceManifest (Path.Combine(evidencePath,"manifest.json")) {
                evidenceFormat=JdfPostInference.EvidenceFormat;schemaVersion=JdfPostInference.EvidenceSchemaVersion
                packId=JdfPostInference.evidencePackId "test-tool" (String.replicate 64 "0") (String.replicate 64 "1")
                captureToolVersion="test-tool"
                mergedJdfSha256=String.replicate 64 "0";routingPbfSha256=String.replicate 64 "1"
                osmSnapshot=None;routerEvidenceVersion=JdfPostInference.RouterEvidenceVersion
                variantEnumerationVersion=JdfPostInference.VariantEnumerationVersion
                captureCeilings={routedExcessMetres=1000.0;maximumCorridorVariants=3}
                maximumSearchStates=100000;maximumSearchDistanceMetres=30000.0
                contextCount=0L;routePointCount=0L;observationCount=0L
                corridorVariantCount=0L;routePointEvidenceCount=0L
                files=evidenceFiles |> Array.map(fun path ->
                    {JdfPostInference.EvidenceFileManifest.path=path;sha256=emptyHash;bytes=0L
                     rows=0L;schemaFingerprint="test"}) }
            let options={ JdfBundle.defaultBundleExecutionOptions with postInferenceEvidencePath=Some evidencePath }
            let error=Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfBundle.writeBundleWithRoutedPostInferenceOptions
                    descriptorPath "test-commit" false JdfToGtfs.KeepAll [||]
                    JdfToGtfs.emptyTransportModeRules true None false options fixturePath
                    (Path.Combine(root,"bundle")))
            StringAssert.Contains(error.Message,"different merged JDF")
        finally
            if Directory.Exists(root) then Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Evidence validation does not accept legacy derived scores``() =
        let root = Path.Combine(Path.GetTempPath(), "jrutil-evidence-no-legacy-" + Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            JdfPostInference.writeEvidenceManifest (Path.Combine(root,"manifest.json")) {
                evidenceFormat=JdfPostInference.EvidenceFormat;schemaVersion=JdfPostInference.EvidenceSchemaVersion
                packId=JdfPostInference.evidencePackId "test-tool" (String.replicate 64 "0") (String.replicate 64 "1")
                captureToolVersion="test-tool"
                mergedJdfSha256=String.replicate 64 "0";routingPbfSha256=String.replicate 64 "1"
                osmSnapshot=None;routerEvidenceVersion=JdfPostInference.RouterEvidenceVersion
                variantEnumerationVersion=JdfPostInference.VariantEnumerationVersion
                captureCeilings={routedExcessMetres=1000.0;maximumCorridorVariants=3}
                maximumSearchStates=100000;maximumSearchDistanceMetres=30000.0
                contextCount=0L;routePointCount=0L;observationCount=0L
                corridorVariantCount=0L;routePointEvidenceCount=0L;files=[||] }
            for fileName in [|"contexts.parquet";"route_points.parquet";"observations.parquet";
                               "derived_post_scores.parquet"|] do
                File.WriteAllBytes(Path.Combine(root,fileName),[||])
            let error=Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfPostInference.validateEvidencePack root |> ignore)
            StringAssert.Contains(error.Message,"observations.parquet")
        finally
            if Directory.Exists(root) then Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Evidence v1 is rejected before any Parquet file is opened``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-evidence-v1-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            File.WriteAllText(Path.Combine(root,"manifest.json"),
                "{\"evidence_format\":\"post-inference-evidence-v1\",\"schema_version\":1}")
            let error=Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfPostInference.validateEvidencePack root |> ignore)
            StringAssert.Contains(error.Message,"v1")
        finally
            Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Capture tool version participates in evidence pack identity``() =
        let merged=String.replicate 64 "0"
        let routing=String.replicate 64 "1"
        let first=JdfPostInference.evidencePackId "capture-tool-a" merged routing
        let second=JdfPostInference.evidencePackId "capture-tool-b" merged routing
        Assert.AreNotEqual(first,second)

    [<TestMethod>]
    member _.``Default post-inference policy is the tuned best-safe policy``() =
        let policy=JdfPostInferencePolicy.conservativeRoutedV4
        assertEqual "conservative-routed-v4|tuned-safe-v3|kostany-diagnostic-best-safe" policy.policyId
        assertEqual 2.0 policy.consolidation.maximumDiameterMetres
        assertEqual 0.90 policy.resolution.minimumPlausibleScore
        assertEqual 0.98 policy.resolution.minimumPhysicalScore
        assertEqual 0.02 policy.resolution.minimumPhysicalMargin
        assertEqual 0.05 policy.resolution.materialRoutedAdvantage
        assertEqual 1 policy.consensus.minimumContexts
        assertEqual 0.67 policy.consensus.minimumWinningShare
        assertEqual 0.0 policy.alternativeCorridors.absoluteTieMetres
        assertEqual 0.02 policy.alternativeCorridors.relativeTieFraction
        assertEqual 75.0 policy.sideGroups.maximumCompactnessMetres
        assertEqual 2 policy.sideGroups.additionalGroupMinimumContexts

    [<TestMethod>]
    member _.``Every policy sweep key changes exactly its named field``() =
        assertEqual 47 JdfPostInferencePolicy.PolicySweepFields.Length
        let options=JsonSerializerOptions(PropertyNamingPolicy=JsonNamingPolicy.SnakeCaseLower)
        let flatten policy =
            use document=JsonDocument.Parse(JsonSerializer.Serialize(policy,options))
            let rec visit prefix (element:JsonElement) = seq {
                for property in element.EnumerateObject() do
                    let path=if prefix="" then property.Name else prefix+"."+property.Name
                    if property.Value.ValueKind=JsonValueKind.Object then
                        yield! visit path property.Value
                    else yield path,property.Value.GetRawText() }
            visit "" document.RootElement |> Map.ofSeq
        let baseline=JdfPostInferencePolicy.conservativeRoutedV4
        let baselineFields=flatten baseline
        let integerFields=Set.ofArray [|
            "consolidation.distinguished_context_minimum_count";"consensus.minimum_contexts"
            "established_post.minimum_supporting_contexts";"alternative_corridors.maximum_variants"
            "side_groups.maximum_ordinary_groups";"side_groups.additional_group_minimum_contexts" |]
        for key in JdfPostInferencePolicy.PolicySweepFields do
            let raw =
                if key="authored_resolution.require_unanimous_contexts" || key="same_stop_pairs.enabled" then
                    if baselineFields.[key]="true" then "false" else "true"
                elif integerFields.Contains key then string(Int32.Parse(baselineFields.[key])+1)
                else
                    let current=Double.Parse(baselineFields.[key],Globalization.CultureInfo.InvariantCulture)
                    (if current=0.0 then 0.001 else current+0.001)
                    |> fun value -> value.ToString("R",Globalization.CultureInfo.InvariantCulture)
            use valueDocument=JsonDocument.Parse(raw)
            let changed=JdfPostInferencePolicy.applySweepValue key valueDocument.RootElement baseline |> flatten
            let differences =
                baselineFields
                |> Seq.choose(fun pair -> if changed.[pair.Key]<>pair.Value then Some pair.Key else None)
                |> Seq.toArray
            CollectionAssert.AreEqual([|key|],differences,key)

    [<TestMethod>]
    member _.``Policy v2 round-trips snake-case fields and rejects non-finite domains``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-policy-v2-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let path=Path.Combine(root,"policy.json")
            let baseline=JdfPostInferencePolicy.conservativeRoutedV4
            JdfPostInferencePolicy.writePolicy path baseline
            assertEqual baseline (JdfPostInferencePolicy.loadPolicy path)
            use document=JsonDocument.Parse(File.ReadAllText(path))
            Assert.IsTrue(document.RootElement.TryGetProperty("same_stop_pairs") |> fst)
            Assert.IsFalse(document.RootElement.TryGetProperty("sameStopPairs") |> fst)
            let invalid={baseline with consolidation={baseline.consolidation with maximumDiameterMetres=Double.PositiveInfinity}}
            let error=Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfPostInferencePolicy.validatePolicy JdfPostInference.CaptureRoutedExcessHorizonMetres invalid |> ignore)
            StringAssert.Contains(error.Message,"finite")
            let invalidPair={baseline with sameStopPairs={baseline.sameStopPairs with minimumIndividualMargin=1.01}}
            Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfPostInferencePolicy.validatePolicy JdfPostInference.CaptureRoutedExcessHorizonMetres invalidPair |> ignore)
            |> ignore
        finally
            Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Evidence manifest rejects extra v2 fields before Parquet access``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-evidence-shape-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let path=Path.Combine(root,"manifest.json")
            JdfPostInference.writeEvidenceManifest path {
                evidenceFormat=JdfPostInference.EvidenceFormat;schemaVersion=JdfPostInference.EvidenceSchemaVersion
                packId=JdfPostInference.evidencePackId "test-tool" (String.replicate 64 "0") (String.replicate 64 "1")
                captureToolVersion="test-tool";mergedJdfSha256=String.replicate 64 "0"
                routingPbfSha256=String.replicate 64 "1";osmSnapshot=None
                routerEvidenceVersion=JdfPostInference.RouterEvidenceVersion
                variantEnumerationVersion=JdfPostInference.VariantEnumerationVersion
                captureCeilings={routedExcessMetres=1000.0;maximumCorridorVariants=3}
                maximumSearchStates=100000;maximumSearchDistanceMetres=30000.0
                contextCount=0L;routePointCount=0L;observationCount=0L
                corridorVariantCount=0L;routePointEvidenceCount=0L;files=[||] }
            let text=File.ReadAllText(path)
            File.WriteAllText(path,text.Insert(text.IndexOf('{')+1,"\"unexpected\":true,"))
            let error=Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfPostInference.loadEvidenceManifest root |> ignore)
            StringAssert.Contains(error.Message,"shape")
        finally
            Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Alternative corridor thresholds reinterpret captured costs without routing``() =
        let facts:JdfPostInference.AlternativeCorridorFact array=[|
            {variantRank=0;relativeCostMetres=Some 0.0;relativeCostFraction=Some 0.0}
            {variantRank=1;relativeCostMetres=Some 20.0;relativeCostFraction=Some 0.20}
            {variantRank=2;relativeCostMetres=Some 100.0;relativeCostFraction=Some 0.01}|]
        let baseline=permissiveCoveragePolicy
        CollectionAssert.AreEqual(
            [|0;1;2|],JdfPostInference.selectedCorridorVariantRanks baseline facts)
        let tight={baseline with alternativeCorridors={baseline.alternativeCorridors with
                                                            absoluteTieMetres=1.0;relativeTieFraction=0.001}}
        CollectionAssert.AreEqual([|0|],JdfPostInference.selectedCorridorVariantRanks tight facts)
        let limited={baseline with alternativeCorridors={baseline.alternativeCorridors with maximumVariants=2}}
        CollectionAssert.AreEqual([|0;1|],JdfPostInference.selectedCorridorVariantRanks limited facts)

    [<TestMethod>]
    member _.``Every initial policy-grid field crosses its production behavior boundary``() =
        let baseline=permissiveCoveragePolicy
        let tightDiameter={baseline with consolidation={baseline.consolidation with maximumDiameterMetres=10.0}}
        let wideDiameter={baseline with consolidation={baseline.consolidation with maximumDiameterMetres=25.0}}
        Assert.IsFalse(JdfPostInference.withinConsolidationDiameter tightDiameter 20.0)
        Assert.IsTrue(JdfPostInference.withinConsolidationDiameter wideDiameter 20.0)

        let facts:JdfPostInference.PhysicalResolutionFacts = {
            finalScore=0.9;margin=0.2;routedAdvantage=0.0;geometryMargin=0.0
            contradictory=false;contextCount=1;winnerShare=1.0
            establishedStrong=false;hasAlternativeCorridors=false;alternativesAgree=true }
        let lowScore={baseline with resolution={baseline.resolution with minimumPhysicalScore=0.70}}
        let highScore={baseline with resolution={baseline.resolution with minimumPhysicalScore=0.95}}
        Assert.IsTrue(JdfPostInference.physicalResolutionEligible lowScore facts)
        Assert.IsFalse(JdfPostInference.physicalResolutionEligible highScore facts)

        let marginFacts={facts with margin=0.12;routedAdvantage=0.0}
        let lowMargin={baseline with resolution={baseline.resolution with minimumPhysicalMargin=0.10
                                                                          materialRoutedAdvantage=0.50}}
        let highMargin={lowMargin with resolution={lowMargin.resolution with minimumPhysicalMargin=0.15}}
        Assert.IsTrue(JdfPostInference.physicalResolutionEligible lowMargin marginFacts)
        Assert.IsFalse(JdfPostInference.physicalResolutionEligible highMargin marginFacts)

        let routedFacts={facts with margin=0.0;routedAdvantage=0.075}
        let lowRouted={baseline with resolution={baseline.resolution with materialRoutedAdvantage=0.05}}
        let highRouted={baseline with resolution={baseline.resolution with materialRoutedAdvantage=0.10}}
        Assert.IsTrue(JdfPostInference.physicalResolutionEligible lowRouted routedFacts)
        Assert.IsFalse(JdfPostInference.physicalResolutionEligible highRouted routedFacts)

        let lowPlausible={baseline with resolution={baseline.resolution with minimumPlausibleScore=0.35}}
        let highPlausible={baseline with resolution={baseline.resolution with minimumPlausibleScore=0.45}}
        Assert.IsTrue(JdfPostInference.plausibleCandidate lowPlausible 0.7 0.65 0.40)
        Assert.IsFalse(JdfPostInference.plausibleCandidate highPlausible 0.7 0.65 0.40)

        let consensusFacts={facts with contextCount=3;winnerShare=0.8}
        let lowShare={baseline with consensus={baseline.consensus with minimumWinningShare=0.67}}
        let unanimous={baseline with consensus={baseline.consensus with minimumWinningShare=1.0}}
        Assert.IsTrue(JdfPostInference.physicalResolutionEligible lowShare consensusFacts)
        Assert.IsFalse(JdfPostInference.physicalResolutionEligible unanimous consensusFacts)

    [<TestMethod>]
    member _.``Every mandatory policy subsystem has a production boundary regression``() =
        let baseline=JdfPostInferencePolicy.conservativeRoutedV4

        let consolidation={baseline with consolidation={baseline.consolidation with
                                                            maximumChainageDifferenceMetres=5.0
                                                            maximumHeadingDifferenceDegrees=10.0
                                                            oppositeSideToleranceMetres=1.0
                                                            distinguishedContextMinimumCount=2
                                                            distinguishedContextMinimumScore=0.6
                                                            distinguishedContextMinimumMargin=0.2}}
        Assert.IsFalse(JdfPostInference.consolidationChainageCompatible consolidation 6.0)
        Assert.IsTrue(JdfPostInference.consolidationChainageCompatible consolidation 5.0)
        Assert.IsFalse(JdfPostInference.consolidationHeadingCompatible consolidation 11.0)
        Assert.IsTrue(JdfPostInference.consolidationHeadingCompatible consolidation 10.0)
        Assert.IsFalse(JdfPostInference.consolidationSidesCompatible consolidation (Some -2.0) (Some 2.0))
        Assert.IsTrue(JdfPostInference.consolidationSidesCompatible consolidation (Some -0.5) (Some 2.0))
        Assert.IsTrue(JdfPostInference.consolidationContextDistinguishes consolidation 0.8 0.5)
        Assert.IsFalse(JdfPostInference.consolidationContextDistinguishes consolidation 0.7 0.55)
        Assert.IsTrue(JdfPostInference.consolidationDistinguishedCountCompatible consolidation 1)
        Assert.IsFalse(JdfPostInference.consolidationDistinguishedCountCompatible consolidation 2)

        let alignmentHeavy={baseline with geometryWeights={alignment=0.5;side=0.15;proximity=0.2;routedExcess=0.15}}
        let baselineAlignment=JdfPostInference.geometryScore baseline 1.0 0.0 0.0 0.0
        let changedAlignment=JdfPostInference.geometryScore alignmentHeavy 1.0 0.0 0.0 0.0
        Assert.IsTrue(changedAlignment > baselineAlignment)
        let sideHeavy={baseline with geometryWeights={alignment=0.3;side=0.35;proximity=0.2;routedExcess=0.15}}
        let baselineSide=JdfPostInference.geometryScore baseline 0.0 1.0 0.0 0.0
        let changedSide=JdfPostInference.geometryScore sideHeavy 0.0 1.0 0.0 0.0
        Assert.IsTrue(changedSide > baselineSide)
        let proximityHeavy={baseline with geometryWeights={alignment=0.3;side=0.25;proximity=0.3;routedExcess=0.15}}
        let baselineProximity=JdfPostInference.geometryScore baseline 0.0 0.0 1.0 0.0
        let changedProximity=JdfPostInference.geometryScore proximityHeavy 0.0 0.0 1.0 0.0
        Assert.IsTrue(changedProximity > baselineProximity)
        let routedHeavy={baseline with geometryWeights={alignment=0.3;side=0.25;proximity=0.2;routedExcess=0.25}}
        let baselineRouted=JdfPostInference.geometryScore baseline 0.0 0.0 0.0 1.0
        let changedRouted=JdfPostInference.geometryScore routedHeavy 0.0 0.0 0.0 1.0
        Assert.IsTrue(changedRouted > baselineRouted)

        let gates={baseline with hardGates={centrelineNeutralToleranceMetres=1.0
                                            maximumCorridorDistanceMetres=10.0
                                            maximumRoutedExcessMetres=20.0}}
        assertEqual (Some "corridor-distance")
                    (JdfPostInference.hardGateReason gates None (Some 11.0) None None)
        assertEqual (Some "routed-excess")
                    (JdfPostInference.hardGateReason gates None (Some 1.0) None (Some 21.0))
        assertEqual (Some "confident-left-side")
                    (JdfPostInference.hardGateReason gates None (Some 1.0) (Some 2.0) (Some 1.0))
        assertEqual (Some "topology")
                    (JdfPostInference.hardGateReason gates (Some "topology") None None None)

        let isolation={baseline with spatialIsolation={minimumSeparationMetres=10.0;maximumAdjustment=0.2}}
        Assert.AreEqual(0.1,JdfPostInference.spatialIsolationAdjustment isolation (Some 5.0),1e-12)
        Assert.AreEqual(0.2,JdfPostInference.spatialIsolationAdjustment isolation (Some 20.0),1e-12)
        Assert.AreEqual(0.0,JdfPostInference.spatialIsolationAdjustment isolation None,1e-12)

        let consensus={baseline with consensus={minimumContexts=3;minimumWinningShare=0.75
                                                minimumPerContextScore=0.8;minimumPerContextLead=0.1}}
        Assert.IsFalse(JdfPostInference.consensusSatisfied consensus 3 0.7 false)
        Assert.IsTrue(JdfPostInference.consensusSatisfied consensus 3 0.8 false)
        Assert.IsFalse(JdfPostInference.consensusSatisfied consensus 2 1.0 false)
        Assert.IsTrue(JdfPostInference.contradictoryContext consensus 0.9 0.2)
        Assert.IsFalse(JdfPostInference.contradictoryContext consensus 0.7 0.2)
        Assert.IsFalse(JdfPostInference.contradictoryContext consensus 0.9 0.05)

        let established={baseline with establishedPost={minimumSupportingContexts=3
                                                        minimumRunnerUpRatio=2.0
                                                        maximumGeometryDisadvantage=0.1
                                                        maximumPopularityAdjustment=0.08
                                                        perContextAdjustment=0.03}}
        Assert.IsTrue(JdfPostInference.establishedPostStrong established 4 2 -0.1 false false true)
        Assert.IsFalse(JdfPostInference.establishedPostStrong established 2 1 0.0 false false true)
        Assert.IsFalse(JdfPostInference.establishedPostStrong established 4 3 0.0 false false true)
        Assert.IsFalse(JdfPostInference.establishedPostStrong established 4 2 -0.11 false false true)
        Assert.IsFalse(JdfPostInference.establishedPostStrong established 4 2 0.0 true false true)
        Assert.IsFalse(JdfPostInference.establishedPostStrong established 4 2 0.0 false true false)
        Assert.AreEqual(0.06,JdfPostInference.establishedPopularityAdjustment established 2,1e-12)
        Assert.AreEqual(0.08,JdfPostInference.establishedPopularityAdjustment established 9,1e-12)

        let modality={baseline with modality={sourceSupportPerWeight=0.02
                                              maximumSourceSupportAdjustment=0.06
                                              explicitSupportAdjustment=0.07
                                              estimatedSupportAdjustment=0.04
                                              conflictAdjustment = -0.08
                                              maximumCombinedSupportingAdjustment=0.09}}
        assertEqual (0.06,0.07)
                    (JdfPostInference.modalitySupportAdjustments modality "A" 10.0 [|"ROAD"|] [||])
        assertEqual (0.0,0.04)
                    (JdfPostInference.modalitySupportAdjustments modality "A" 1.0 [||] [||])
        assertEqual (0.0,-0.08)
                    (JdfPostInference.modalitySupportAdjustments modality "A" 1.0 [|"ROAD"|] [|"BUS"|])
        Assert.AreEqual(0.09,JdfPostInference.combinedSupportAdjustment modality 0.06 0.07,1e-12)
        Assert.AreEqual(-0.08,JdfPostInference.combinedSupportAdjustment modality 0.0 -0.5,1e-12)

        let sideGroups={baseline with sideGroups={maximumOrdinaryGroups=1
                                                  maximumCompactnessMetres=20.0
                                                  additionalGroupMinimumContexts=3}}
        Assert.IsTrue(JdfPostInference.sideResolutionEligible sideGroups 2 1 false 1 1 20.0)
        Assert.IsFalse(JdfPostInference.sideResolutionEligible sideGroups 2 1 false 2 2 20.0)
        Assert.IsTrue(JdfPostInference.sideResolutionEligible sideGroups 2 1 false 2 3 20.0)
        Assert.IsFalse(JdfPostInference.sideResolutionEligible sideGroups 2 1 false 1 1 21.0)
        Assert.IsFalse(JdfPostInference.sideResolutionEligible sideGroups 2 2 false 1 1 10.0)
        Assert.IsFalse(JdfPostInference.sideResolutionEligible sideGroups 2 1 true 1 1 10.0)

        let authored={baseline with authoredResolution={minimumPhysicalScore=0.8
                                                        minimumPhysicalMargin=0.2
                                                        requireUnanimousContexts=true}}
        Assert.IsTrue(JdfPostInference.authoredResolutionEligible authored 2 2 1
                          [|0.9,Some 0.3;0.8,Some 0.2|])
        Assert.IsFalse(JdfPostInference.authoredResolutionEligible authored 2 1 1 [|0.9,Some 0.3|])
        Assert.IsFalse(JdfPostInference.authoredResolutionEligible authored 2 2 2 [|0.9,Some 0.3|])
        Assert.IsFalse(JdfPostInference.authoredResolutionEligible authored 2 2 1 [|0.79,Some 0.3|])
        Assert.IsFalse(JdfPostInference.authoredResolutionEligible authored 2 2 1 [|0.9,Some 0.19|])
        Assert.IsTrue(JdfPostInference.authoredResolutionEligible
            {authored with authoredResolution={authored.authoredResolution with requireUnanimousContexts=false}}
            2 1 1 [|0.9,Some 0.3|])

    [<TestMethod>]
    member _.``Same-stop distinctness only breaks a publishable ordinary tie``() =
        let policy=JdfPostInferencePolicy.conservativeRoutedV4.sameStopPairs
        let choice candidate score geometry margin publishable:JdfPostInference.SameStopPairChoice = {
            candidateId=candidate;ordinaryScore=score;ordinaryGeometry=geometry
            individualMargin=margin;independentlyPublishable=publishable }
        let tied=[|choice "a" 0.8 0.7 0.2 true;choice "b" 0.8 0.7 0.2 true|]
        assertEqual (Some("a","b")) (JdfPostInference.selectDistinctSameStopPair policy tied tied)
        let better=[|choice "a" 0.9 0.7 0.2 true;choice "b" 0.8 0.7 0.2 true|]
        assertEqual None (JdfPostInference.selectDistinctSameStopPair policy better better)
        let geometryWinner=[|choice "a" 0.8 0.8 0.2 true;choice "b" 0.8 0.7 0.2 true|]
        assertEqual None (JdfPostInference.selectDistinctSameStopPair policy geometryWinner geometryWinner)
        let gated=[|choice "a" 0.8 0.7 0.2 true;choice "b" 0.8 0.7 0.2 false|]
        assertEqual None (JdfPostInference.selectDistinctSameStopPair policy gated gated)
        let belowScore=[|choice "a" (policy.minimumIndividualScore-0.01) 0.7 0.2 true
                         choice "b" (policy.minimumIndividualScore-0.01) 0.7 0.2 true|]
        assertEqual None (JdfPostInference.selectDistinctSameStopPair policy belowScore belowScore)
        let belowMargin=[|choice "a" 0.8 0.7 (policy.minimumIndividualMargin-0.01) true
                          choice "b" 0.8 0.7 (policy.minimumIndividualMargin-0.01) true|]
        assertEqual None (JdfPostInference.selectDistinctSameStopPair policy belowMargin belowMargin)
        assertEqual (Some("a","b"))
                    (JdfPostInference.selectDistinctSameStopPair policy (Array.rev tied) (Array.rev tied))
        assertEqual None (JdfPostInference.selectDistinctSameStopPair {policy with enabled=false} tied tied)

        let ordinary:JdfPostInference.SameStopBlockFacts = {
            contextCount=2;leftAssignmentKind="unlabelled";rightAssignmentKind="unlabelled"
            leftMovementFamilyId="left";rightMovementFamilyId="right"
            leftResolution="Physical";rightResolution="Physical"
            leftCandidateId=Some "a";rightCandidateId=Some "a" }
        Assert.IsTrue(JdfPostInference.sameStopDistinctnessEligible ordinary)
        Assert.IsFalse(JdfPostInference.sameStopDistinctnessEligible {ordinary with contextCount=3})
        Assert.IsFalse(JdfPostInference.sameStopDistinctnessEligible
            {ordinary with leftAssignmentKind="authored"})
        Assert.IsFalse(JdfPostInference.sameStopDistinctnessEligible
            {ordinary with rightAssignmentKind="authored"})
        Assert.IsFalse(JdfPostInference.sameStopDistinctnessEligible
            {ordinary with rightMovementFamilyId="left"})
        Assert.IsFalse(JdfPostInference.sameStopDistinctnessEligible
            {ordinary with rightResolution="Side"})
        Assert.IsFalse(JdfPostInference.sameStopDistinctnessEligible
            {ordinary with rightCandidateId=Some "b"})
        Assert.IsFalse(JdfPostInference.sameStopDistinctnessEligible
            {ordinary with leftCandidateId=None;rightCandidateId=None})

    [<TestMethod>]
    member _.``Replayable row stores repeat and clean deterministic binary spills``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-result-spool-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            use memory=JdfPostInference.ReplayableRowStore<int>.Create(4096L,root,[1..10])
            assertEqual 10L memory.Count
            assertEqual 0L memory.CurrentSpillBytes
            CollectionAssert.AreEqual([|1..10|],memory.ReadRows() |> Seq.toArray)
            CollectionAssert.AreEqual([|1..10|],memory.ReadRows() |> Seq.toArray)

            let spilled=JdfPostInference.ReplayableRowStore<int>.Create(1L,root,[1..100])
            assertEqual 100L spilled.Count
            Assert.IsTrue(spilled.CurrentSpillBytes>0L)
            assertEqual spilled.CurrentSpillBytes spilled.PeakSpillBytes
            CollectionAssert.AreEqual([|1..100|],spilled.ReadRows() |> Seq.toArray)
            CollectionAssert.AreEqual([|1..100|],spilled.ReadRows() |> Seq.toArray)
            spilled.Dispose()
            assertEqual 0L spilled.CurrentSpillBytes
            assertEqual 0 (Directory.GetFiles(root).Length)

            let sorted=JdfPostInference.ReplayableRowStore<int>.CreateSorted(
                32L,root,id,[100..-1..1])
            CollectionAssert.AreEqual([|1..100|],sorted.ReadRows() |> Seq.toArray)
            CollectionAssert.AreEqual([|1..100|],sorted.ReadRows() |> Seq.toArray)
            Assert.IsTrue(sorted.PeakSpillBytes>0L)
            sorted.Dispose()
            assertEqual 0 (Directory.GetFiles(root).Length)

            Assert.ThrowsExactly<Exception>(fun () ->
                JdfPostInference.ReplayableRowStore<int>.Create(
                    1L,root,seq { yield 1; failwith "injected row failure" })
                |> ignore)
            |> ignore
            assertEqual 0 (Directory.GetFiles(root).Length)

            Assert.ThrowsExactly<Exception>(fun () ->
                JdfPostInference.ReplayableRowStore<int>.CreateSorted(
                    8L,root,id,seq {
                        for value in 100..-1..50 do yield value
                        failwith "injected external-sort failure" })
                |> ignore)
            |> ignore
            assertEqual 0 (Directory.GetFiles(root).Length)
        finally
            Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Complete inference result adapts without re-evaluating and owns cleanup``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-result-adapter-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let contextId="context:test"
            let locationId="post:1:physical:h1"
            let assignment kind authoredKey position:JdfPostInference.ContextPostAssignment = {
                contextId=if kind="authored" then contextId+":authored" else contextId
                stopId=1L;mode="A";lineId="100";routeDistinction=0;direction=0
                patternHash="pattern";patternPosition=position
                previousStopId=Some 2L;nextStopId=Some 3L
                assignmentKind=kind;authoredPostKey=authoredKey;sameStopBlockId=None
                sameStopBlockRole="through";movementFamilyId="family"
                resolution="Physical";selectedLocationId=Some locationId
                selectedHypothesisId=Some "h1";selectedSideGroupId=None
                score=Some 0.9;margin=Some 0.3 }
            let assignmentStore=
                JdfPostInference.ReplayableRowStore<JdfPostInference.ContextPostAssignment>.Create(
                    1L,root,[assignment "unlabelled" None 0;assignment "authored" (Some "id:7") 1])
            let diagnostic:JdfPostInference.PostInferenceDiagnosticScore = {
                contextId=contextId;candidateId="h1";variantRank=0;eligible=true
                alignment=0.8;side=0.7;proximity=0.6;routedExcess=0.5
                routedExcessMetres=Some 10.0;corridorId=Some "corridor"
                ingressThreadId=Some "in";egressThreadId=Some "out"
                corridorFaceId=Some "face";routingAvailability="available"
                alternativeCorridorCount=2;alternativeCostGap=Some 5.0
                selectedCorridorRanks=[|0;1|];tiedCorridorsAgree=true
                corridorDistance=Some 3.0;signedLateralOffset=Some -2.0
                corridorHeading=Some 0.0;attachmentHeading=Some 1.0
                snapEdgeId=Some 11;snapFraction=Some 0.25
                topologyFailureReason=None;sourceAdjustment=0.02
                modalityAdjustment=0.03;popularityAdjustment=0.04
                total=0.91;rejectionReason=None }
            let diagnosticStore=
                JdfPostInference.ReplayableRowStore<JdfPostInference.PostInferenceDiagnosticScore>.Create(
                    1L,root,[diagnostic])
            let hypothesis:JdfPostInference.ConsolidatedPostHypothesis =
                { hypothesisId="h1";stopId=1L;representativeRoutePointId="r1";
                    memberRoutePointIds=[|"r1";"r2"|];memberObservationIds=[|"o1";"o2"|];
                    latitude=50.0;longitude=14.0 }
            let authoredPosition:JdfPostInference.AuthoredPostPosition =
                { stopId=1L;authoredPostKey="id:7";locationId=locationId;
                  latitude=Some 50.0;longitude=Some 14.0 }
            let result=new JdfPostInference.PostInferenceResult(
                [|hypothesis|],
                [||],assignmentStore,
                [|authoredPosition|],
                diagnosticStore,
                { evidenceRows=1L;contextCount=2;candidateStopCount=1
                  unresolvedContexts=0;authoredPositions=1;sameStopBlocks=0
                  distinctPairChoices=0;unresolvedBlockEdges=0
                  physicalResolutions=1;sideResolutions=0;centroidResolutions=0 })
            let plan=JdfToGtfs.postEstimationPlanFromInferenceResult result
            assertEqual 1 plan.calls.Count
            assertEqual 1 plan.authored.Count
            assertEqual "id:7" (plan.authored |> Seq.exactlyOne |> _.Key |> snd)
            assertEqual [|"o1";"o2"|] plan.physicalHypotheses.[0].memberObservationIds
            let score=plan.scoreRows() |> Seq.exactlyOne
            assertEqual (Some "corridor") score.corridorId
            assertEqual (Some 11) score.snapEdgeId
            assertEqual 0.04 score.popularityPrior
            Assert.IsTrue(assignmentStore.CurrentSpillBytes>0L)
            Assert.IsTrue(diagnosticStore.CurrentSpillBytes>0L)
            plan.cleanupScoreRows()
            assertEqual 0L assignmentStore.CurrentSpillBytes
            assertEqual 0L diagnosticStore.CurrentSpillBytes
            assertEqual 0 (Directory.GetFiles(root).Length)
        finally
            if Directory.Exists(root) then Directory.Delete(root,true)

    [<TestMethod>]
    member _.``Evidence file tampering is rejected by the manifest hash``() =
        let root=Path.Combine(Path.GetTempPath(),"jrutil-evidence-tamper-"+Guid.NewGuid().ToString("N"))
        Directory.CreateDirectory(root) |> ignore
        try
            let emptyHash="e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
            for fileName in JdfPostInference.RequiredEvidenceFiles do
                File.WriteAllBytes(Path.Combine(root,fileName),[||])
            let manifest:JdfPostInference.PostInferenceEvidenceManifest = {
                evidenceFormat=JdfPostInference.EvidenceFormat;schemaVersion=JdfPostInference.EvidenceSchemaVersion
                packId=JdfPostInference.evidencePackId "test-tool" (String.replicate 64 "0") (String.replicate 64 "1")
                captureToolVersion="test-tool"
                mergedJdfSha256=String.replicate 64 "0";routingPbfSha256=String.replicate 64 "1"
                osmSnapshot=None;routerEvidenceVersion=JdfPostInference.RouterEvidenceVersion
                variantEnumerationVersion=JdfPostInference.VariantEnumerationVersion
                captureCeilings={routedExcessMetres=1000.0;maximumCorridorVariants=3}
                maximumSearchStates=100000;maximumSearchDistanceMetres=30000.0
                contextCount=0L;routePointCount=0L;observationCount=0L
                corridorVariantCount=0L;routePointEvidenceCount=0L
                files=JdfPostInference.RequiredEvidenceFiles |> Array.map(fun path ->
                    {JdfPostInference.EvidenceFileManifest.path=path;sha256=emptyHash;bytes=0L
                     rows=0L;schemaFingerprint="fixture"}) }
            JdfPostInference.writeEvidenceManifest (Path.Combine(root,"manifest.json")) manifest
            JdfPostInference.validateEvidencePack root |> ignore
            JdfPostInference.writeEvidenceManifest (Path.Combine(root,"manifest.json"))
                { manifest with observationCount=1 }
            let countError=Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfPostInference.validateEvidencePack root |> ignore)
            StringAssert.Contains(countError.Message,"row-count metadata")
            JdfPostInference.writeEvidenceManifest (Path.Combine(root,"manifest.json")) manifest
            File.WriteAllBytes(Path.Combine(root,"observations.parquet"),[|1uy|])
            let error=Assert.ThrowsExactly<ArgumentException>(fun () ->
                JdfPostInference.validateEvidencePack root |> ignore)
            StringAssert.Contains(error.Message,"size mismatch")
        finally
            Directory.Delete(root,true)
