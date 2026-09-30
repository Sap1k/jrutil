// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// Serilog setup and logging helpers.
module JrUtil.Logging

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

type internal SynchronizedConsoleSink(inner: Serilog.ILogger) =
    let syncRoot = obj()

    interface ILogEventSink with
        member _.Emit(logEvent) =
            lock syncRoot (fun () ->
                match logEvent.Properties.TryGetValue("JrUtilProgressEvent") with
                | true, (:? ScalarValue as value) ->
                    match value.Value with
                    | :? string as line ->
                        Console.Error.WriteLine(line)
                        Console.Error.Flush()
                    | _ -> inner.Write(logEvent)
                | _ -> inner.Write(logEvent))

    interface IDisposable with
        member _.Dispose() =
            match box inner with
            | :? IDisposable as disposable -> disposable.Dispose()
            | _ -> ()

let setupLogging (logFile: string option) () =
    let mutable loggerFactory =
        LoggerConfiguration()
         .MinimumLevel.Debug()
         .Enrich.FromLogContext()
    if Environment.GetEnvironmentVariable("JRUTIL_LOG_TO_CONSOLE") <> "0" then
        let consoleLogger =
            LoggerConfiguration()
             .MinimumLevel.Verbose()
             .WriteTo.Console(
                 standardErrorFromLevel = LogEventLevel.Verbose,
                 applyThemeToRedirectedOutput = true,
                 theme =
                     if RuntimeInformation.IsOSPlatform(OSPlatform.Windows)
                         then SystemConsoleTheme.Literate :> ConsoleTheme
                     else AnsiConsoleTheme.Literate :> ConsoleTheme)
             .CreateLogger()
        loggerFactory <-
            loggerFactory.WriteTo.Sink(new SynchronizedConsoleSink(consoleLogger))
    logFile |> Option.iter (fun lf ->
        if lf.StartsWith("display:") then
            loggerFactory <- loggerFactory.WriteTo.File(lf.[8..])
        else if lf.StartsWith("json:") then
            loggerFactory <- loggerFactory.WriteTo.File(
                CompactJsonFormatter(), lf.[5..])
        else if lf.StartsWith("rjson:") then
            loggerFactory <- loggerFactory.WriteTo.File(
                RenderedCompactJsonFormatter(), lf.[6..])
        else
            loggerFactory <- loggerFactory.WriteTo.File(lf))
    Log.Logger <- loggerFactory.CreateLogger()

/// Create Serilog event without logging it immediately
let logEvent level (msg: string) (props: 'a array) =
    let valid, parsedMsg, boundProps =
        Log.BindMessageTemplate(msg, props |> Array.map box)
    if not valid then
        failwithf "Invalid log event: '%s' %A" msg props
    LogEvent(DateTimeOffset.Now, level, null, parsedMsg, boundProps)

/// Useful for wrapping long computations for logging, e.g.
/// `logWrappedOp "Computing Pi" (getDigitsOfPi 1000)`
let logWrappedOp (msg: string) f =
    Log.Information("{Operation}...", msg)
    let v = f ()
    Log.Information("{Operation} finished", msg)
    v
