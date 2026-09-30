// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

module JrUtil.Utils

open System
open System.IO
open System.Diagnostics
open System.Globalization
open System.Collections
open System.Collections.Concurrent
open System.Runtime.InteropServices
open System.Runtime.Serialization.Formatters.Binary
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open DocoptNet
open Serilog
open Serilog.Core
open Serilog.Events
open Serilog.Sinks.SystemConsole.Themes
open Serilog.Formatting.Compact
open NodaTime
open NodaTime.Text

#nowarn "0342"

open System

type MultiDict<'k, 'v when 'k: equality>() =
    let dict = Dictionary<'k, ResizeArray<'v>>()

    /// The values of a key; a missing key gets a new empty list.
    member this.Item
        with get k =
            let success, v = dict.TryGetValue(k)
            if success then v
            else
                let arr = ResizeArray()
                dict.[k] <- arr
                arr

    /// Replace the values of a key with a materialised copy of values.
    member this.Replace(k, values: 'v seq) =
        dict.[k] <- ResizeArray(values)

    member this.Keys with get() = dict.Keys
    member this.Values with get() = dict.Values
    member this.Remove(k) = dict.Remove(k)


let memoize f =
    let cache = new ConcurrentDictionary<_, _>()
    fun x ->
        let cached, result = cache.TryGetValue(x)
        if cached then result
        else
            let result = f x
            cache.[x] <- result
            result

let memoizeVoidFunc f =
    let gate = obj()
    let mutable cache = None
    fun () ->
        lock gate (fun () ->
            match cache with
            | Some value -> value
            | None ->
                let value = f()
                cache <- Some value
                value)

/// Groups a sequence whose equal keys are already contiguous. Unlike Seq.groupBy,
/// this keeps only one group in memory, which is important for national call data.
let groupAdjacentBy keySelector (inputs: 'a seq) = seq {
    use enumerator = inputs.GetEnumerator()
    if enumerator.MoveNext() then
        let mutable currentKey = keySelector enumerator.Current
        let mutable current = ResizeArray<'a>()
        current.Add(enumerator.Current)
        while enumerator.MoveNext() do
            let key = keySelector enumerator.Current
            if key = currentKey then
                current.Add(enumerator.Current)
            else
                yield currentKey, current.ToArray()
                currentKey <- key
                current <- ResizeArray<'a>()
                current.Add(enumerator.Current)
        yield currentKey, current.ToArray()
}

let constant x _ = x

let leftJoinOn xkey ykey xs ys =
    let ysMap = ys |> Seq.map (fun y -> ykey y, y) |> Map
    xs |> Seq.map (fun x -> x, ysMap |> Map.tryFind (xkey x))

exception JoinException of string
    with override this.Message = this.Data0

let innerJoinOn xkey ykey xs ys =
    leftJoinOn xkey ykey xs ys
    |> Seq.map (fun (x, yo) ->
        x, match yo with
           | Some y -> y
           | None -> raise (JoinException (sprintf
                "Could not match left key %A" (xkey x))))

let nullOpt v =
    if v = null then None else Some v

let nullableOpt (v: 'a Nullable) = if v.HasValue then Some v.Value else None
