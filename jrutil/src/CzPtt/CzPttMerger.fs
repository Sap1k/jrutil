// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.CzPttMerge

open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography
open System.Text
open System.Xml.Serialization

open JrUtil.CzPtt
open Serilog
open Serilog.Context

type private SpillEntry = {
    offset: int64
    length: int
    fingerprint: string
}

type CzPttMerger(?spillPath: string) =
    let serializer = XmlSerializer(typeof<CzPttXml.CzpttcisMessage>)
    let spillEntries = Dictionary<string, SpillEntry>()
    let sourcePaIds = HashSet<string>()
    let mutable disposed = false
    let spillStream =
        spillPath
        |> Option.map (fun path ->
            let full = Path.GetFullPath(path)
            Directory.CreateDirectory(Path.GetDirectoryName(full)) |> ignore
            File.Open(full, FileMode.CreateNew, FileAccess.ReadWrite, FileShare.Read))

    let getPaidStr (ptis: CzPttXml.TransportIdentifier seq) =
        ptis
        |> Seq.find (fun pti -> pti.ObjectType = CzPttXml.ObjectType.Pa)
        |> identifierStr

    let serializeMessage (msg: CzPttXml.CzpttcisMessage) =
        use writer = new StringWriter()
        serializer.Serialize(writer, msg)
        Encoding.UTF8.GetBytes(writer.ToString())

    let fingerprint (payload: byte array) =
        SHA256.HashData(payload) |> Convert.ToHexString

    let append (payload: byte array) payloadFingerprint =
        let stream = spillStream.Value
        stream.Seek(0L, SeekOrigin.End) |> ignore
        let offset = stream.Position
        stream.Write(payload, 0, payload.Length)
        stream.Flush()
        { offset = offset; length = payload.Length; fingerprint = payloadFingerprint }

    let readEntry entry =
        let payload = Array.zeroCreate<byte> entry.length
        let stream = spillStream.Value
        stream.Seek(entry.offset, SeekOrigin.Begin) |> ignore
        let mutable read = 0
        while read < payload.Length do
            let count = stream.Read(payload, read, payload.Length - read)
            if count = 0 then
                raise (EndOfStreamException("Truncated CZPTT spill file"))
            read <- read + count
        use reader = new StringReader(Encoding.UTF8.GetString(payload))
        serializer.Deserialize(reader) :?> CzPttXml.CzpttcisMessage

    // Indexed by PAID. Retained for compatibility with memory-backed callers.
    member val Messages = Dictionary<string, CzPttXml.CzpttcisMessage>()
    member val UnknownCancellationTargets = Dictionary<string, int>()
    member _.SourcePaIds = sourcePaIds :> seq<string>
    member _.SpillBytes =
        spillStream |> Option.map (fun stream -> stream.Length) |> Option.defaultValue 0L
    member this.SurvivingMessages =
        match spillStream with
        | None -> this.Messages.Values :> seq<_>
        | Some _ -> spillEntries.Values |> Seq.map readEntry

    member private _.Fingerprint(msg: CzPttXml.CzpttcisMessage) =
        msg |> serializeMessage |> fingerprint

    member this.Add(msg: CzPttXml.CzpttcisMessage) =
        let paid = getPaidStr msg.Identifiers
        sourcePaIds.Add(paid) |> ignore
        match spillStream with
        | None ->
            if this.Messages.ContainsKey(paid) then
                if this.Fingerprint(this.Messages.[paid]) = this.Fingerprint(msg) then
                    Log.Information("Ignoring byte-equivalent duplicate message: {PAID}", paid)
                else
                    raise (CzPttInvalidException(
                        $"Conflicting timetable messages share PA identity {paid}"))
            else
                this.Messages.[paid] <- msg
        | Some _ ->
            let payload = serializeMessage msg
            let payloadFingerprint = fingerprint payload
            match spillEntries.TryGetValue(paid) with
            | true, existing when existing.fingerprint = payloadFingerprint ->
                Log.Information("Ignoring byte-equivalent duplicate message: {PAID}", paid)
            | true, _ ->
                raise (CzPttInvalidException(
                    $"Conflicting timetable messages share PA identity {paid}"))
            | _ -> spillEntries.[paid] <- append payload payloadFingerprint

    member this.Cancel(cancelMsg: CzPttXml.CzCanceledPttMessage) =
        let paid = getPaidStr cancelMsg.PlannedTransportIdentifiers
        let exists =
            match spillStream with
            | None -> this.Messages.ContainsKey(paid)
            | Some _ -> spillEntries.ContainsKey(paid)
        if not exists then
            this.UnknownCancellationTargets.[paid] <-
                match this.UnknownCancellationTargets.TryGetValue(paid) with
                | true, count -> count + 1
                | _ -> 1
        else
            let msg =
                match spillStream with
                | None -> this.Messages.[paid]
                | Some _ -> readEntry spillEntries.[paid]
            let msgBitmap = parseCalendar msg.CzpttInformation.PlannedCalendar
            let cancelBitmap = parseCalendar cancelMsg.PlannedCalendar
            // false in cancelBitmap means "no change", so extend by false,
            // invert it and AND it with the original calendar.
            let newBitmap =
                msgBitmap.And(cancelBitmap.ExtendTo(msgBitmap.Interval, false).Not())
            if not (newBitmap.HasAnySet()) then
                match spillStream with
                | None -> this.Messages.Remove(paid) |> ignore
                | Some _ -> spillEntries.Remove(paid) |> ignore
            else
                msg.CzpttInformation.PlannedCalendar <- serializeCalendar(newBitmap)
                match spillStream with
                | None -> ()
                | Some _ ->
                    let payload = serializeMessage msg
                    spillEntries.[paid] <- append payload (fingerprint payload)

    member this.Process(msg: CzpttMessage) =
        match msg with
        | Timetable message -> this.Add(message)
        | Cancellation cancellation -> this.Cancel(cancellation)

    member this.ProcessAll(msgs: (string * CzpttMessage) seq) =
        for name, msg in msgs do
            use _logCtx = LogContext.PushProperty("CzPttFile", name)
            Log.Information("Merging CZPTT file {CzPttFile}", name)
            try
                this.Process(msg)
            with
            | :? CzPttInvalidException -> reraise()
            | error -> Log.Error(error, "Error while merging {CzPttFile}", name)
        if this.UnknownCancellationTargets.Count > 0 then
            let total = this.UnknownCancellationTargets.Values |> Seq.sum
            Log.Warning(
                "{Count} cancellations targeted {DistinctCount} unknown PA identities",
                total, this.UnknownCancellationTargets.Count)

    interface IDisposable with
        member _.Dispose() =
            if not disposed then
                disposed <- true
                spillStream |> Option.iter (fun stream -> stream.Dispose())
                spillPath
                |> Option.iter (fun path ->
                    if File.Exists(path) then File.Delete(path))
