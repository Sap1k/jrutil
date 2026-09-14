// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Serving

open System
open System.IO
open System.Threading
open System.Collections.Generic

module RouteStopWriter =
    [<Struct>]
    type private Row = { route: string; stop: string; location: string }

    /// Collapse repeated call facts in a bounded window before spilling.
    /// Conflicts remain errors both within a window and across merged runs.
    type Writer(path: string, token: CancellationToken, ?maximumBufferBytes: int64) =
        let budget =
            let value = defaultArg maximumBufferBytes (8L * 1024L * 1024L)
            if value <= 0L then invalidArg "maximumBufferBytes" "Route-stop buffer budget must be positive"
            value
        let spoolPath = path + ".facts"
        let spool = new BinaryWriter(File.Create(spoolPath))
        let pending = Dictionary<struct(string * string), string>()
        let mutable bytes = 0L
        let mutable closed = false
        let encode (writer: BinaryWriter) row =
            writer.Write(row.route); writer.Write(row.stop); writer.Write(row.location)
        let decode (reader: BinaryReader) =
            { route = reader.ReadString(); stop = reader.ReadString(); location = reader.ReadString() }
        let size row = 160L + int64 (row.route.Length + row.stop.Length + row.location.Length) * 2L
        let flushFacts () =
            for KeyValue(struct(route, stop), location) in pending do
                token.ThrowIfCancellationRequested()
                encode spool { route = route; stop = stop; location = location }
            pending.Clear()
            bytes <- 0L

        member _.Append(route, stop, location) =
            if closed then raise (ObjectDisposedException("RouteStopWriter"))
            token.ThrowIfCancellationRequested()
            let key = struct(route, stop)
            match pending.TryGetValue(key) with
            | true, prior ->
                if prior <> location then invalidOp $"Conflicting location for route stop {route}/{stop}"
            | _ ->
                let required = size { route = route; stop = stop; location = location }
                if required > budget then invalidOp $"One route-stop record needs {required} bytes; budget is {budget}"
                if pending.Count = 32768 || bytes + required > budget then flushFacts ()
                pending.Add(key, location)
                bytes <- bytes + required

        member _.Complete(progress: string -> int64 -> unit) =
            if closed then raise (ObjectDisposedException("RouteStopWriter"))
            flushFacts ()
            spool.Dispose()
            closed <- true
            let rows = seq {
                use input = new BinaryReader(File.OpenRead(spoolPath))
                while input.BaseStream.Position < input.BaseStream.Length do yield decode input
            }
            let compareRows left right =
                let order = StringComparer.Ordinal.Compare(left.route, right.route)
                if order <> 0 then order else StringComparer.Ordinal.Compare(left.stop, right.stop)
            let schema = Schema.relations |> Array.find (fun relation -> relation.name = "route_stop")
            let columns (rows: Row array) =
                let column field = rows |> Array.map field
                [| ColumnWriter.Text(column _.route); ColumnWriter.Text(column _.stop); ColumnWriter.Text(column _.location) |]
            try
                RelationWriter.write path schema budget 32768 (4L * 1024L * 1024L) 8192
                    token progress size compareRows (=) encode decode columns rows
            finally File.Delete(spoolPath)
        interface IDisposable with
            member _.Dispose() =
                if not closed then
                    closed <- true
                    spool.Dispose()
