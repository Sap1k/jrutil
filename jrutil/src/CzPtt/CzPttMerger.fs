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

type CzPttMerger() =
    let getPaidStr (ptis: CzPttXml.TransportIdentifier seq) =
        ptis |> Seq.find (fun pti ->
            pti.ObjectType = CzPttXml.ObjectType.Pa)
        |> identifierStr

    // Indexed by PAID
    member val Messages = Dictionary<string, CzPttXml.CzpttcisMessage>()
    member val UnknownCancellationTargets = Dictionary<string, int>()

    member private _.Fingerprint(msg: CzPttXml.CzpttcisMessage) =
        let serializer = XmlSerializer(typeof<CzPttXml.CzpttcisMessage>)
        use writer = new StringWriter()
        serializer.Serialize(writer, msg)
        SHA256.HashData(Encoding.UTF8.GetBytes(writer.ToString()))
        |> Convert.ToHexString

    member this.Add(msg: CzPttXml.CzpttcisMessage) =
        let paid = getPaidStr msg.Identifiers
        if this.Messages.ContainsKey(paid) then
            if this.Fingerprint(this.Messages.[paid]) = this.Fingerprint(msg) then
                Log.Information("Ignoring byte-equivalent duplicate message: {PAID}", paid)
            else
                raise (CzPttInvalidException(
                    $"Conflicting timetable messages share PA identity {paid}"))
        else
            this.Messages[paid] <- msg

    member this.Cancel(cancelMsg: CzPttXml.CzCanceledPttMessage) =
        let paid = getPaidStr cancelMsg.PlannedTransportIdentifiers
        if not <| this.Messages.ContainsKey(paid) then
            this.UnknownCancellationTargets.[paid] <-
                (match this.UnknownCancellationTargets.TryGetValue(paid) with
                 | true, count -> count + 1
                 | _ -> 1)
        else
            let msg = this.Messages[paid]
            let msgBitmap = parseCalendar msg.CzpttInformation.PlannedCalendar
            let cancelBitmap = parseCalendar cancelMsg.PlannedCalendar
            // false in cancelBitmap means "no change", so we extend it padding
            // by false, invert it and AND with original
            let newBitmap =
                msgBitmap.And(
                    cancelBitmap.ExtendTo(msgBitmap.Interval, false).Not())
            if newBitmap.HasAnySet() |> not then
                // Train was fully cancelled, remove it entirely
                this.Messages.Remove(paid) |> ignore
            else
                msg.CzpttInformation.PlannedCalendar <-
                    serializeCalendar(newBitmap)
                ()

    member this.Process(msg: CzpttMessage) =
        match msg with
        | Timetable m -> this.Add(m)
        | Cancellation c -> this.Cancel(c)

    member this.ProcessAll(msgs: (string * CzpttMessage) seq) =
        for name, msg in msgs do
            use _logCtx = LogContext.PushProperty("CzPttFile", name)
            Log.Information("Merging CZPTT file {CzPttFile}", name)
            try
                this.Process(msg)
            with
            | :? CzPttInvalidException -> reraise()
            | e -> Log.Error(e, "Error while merging {CzPttFile}", name)
        if this.UnknownCancellationTargets.Count > 0 then
            let total =
                this.UnknownCancellationTargets.Values |> Seq.sum
            Log.Warning(
                "{Count} cancellations targeted {DistinctCount} unknown PA identities",
                total, this.UnknownCancellationTargets.Count)
