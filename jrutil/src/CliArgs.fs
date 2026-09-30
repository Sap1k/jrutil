// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// Docopt argument access.
module JrUtil.CliArgs

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

let argFlagSet (args: IDictionary<string, ArgValue>) name =
    let arg = args.[name]
    let s, v = arg.TryAsBoolean()
    assert s
    v

let argValue (args: IDictionary<string, ArgValue>) name =
    let arg = args.[name]
    let s, v = arg.TryAsString()
    assert s
    v

let optArgValue (args: IDictionary<string, ArgValue>) name =
    match args.TryGetValue(name) with
    | true, a ->
        let s, v = a.TryAsString()
        if s then Some v
        else None
    | false, _ -> None

let argValues (args: IDictionary<string, ArgValue>) name =
    let arg = args.[name]
    let s, v = arg.TryAsStringList()
    assert s
    v

let withProcessedArgs docstring (args: string array) fn =
    match Docopt.CreateParser(docstring)
                .Parse(args) with
    | :? IArgumentsResult<_> as r -> fn r.Arguments
    | :? IHelpResult | :? IVersionResult ->
        printfn "%s" docstring
        0
    | :? IInputErrorResult as e ->
        printfn "%s" e.Error
        1
    | _ -> assert false; 1
