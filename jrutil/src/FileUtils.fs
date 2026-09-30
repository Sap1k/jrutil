// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// File reading, atomic writes and case-insensitive lookup.
module JrUtil.FileUtils

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

let fileLinesSeq filename = seq {
    use file = File.OpenText filename
    while not file.EndOfStream do yield file.ReadLine()
}

/// Creates a unique sibling file, activates it only after the callback
/// succeeds, and removes it on every unsuccessful exit path.
let writeAtomicFile (destination: string) (write: string -> unit) =
    let fullDestination = Path.GetFullPath(destination)
    let parent = Path.GetDirectoryName(fullDestination)
    if String.IsNullOrEmpty(parent) then
        invalidArg "destination" "An atomic output requires a parent directory"
    Directory.CreateDirectory(parent) |> ignore
    let temporary =
        Path.Combine(parent, $".{Path.GetFileName(fullDestination)}.{Guid.NewGuid():N}.part")
    let mutable completed = false
    try
        write temporary
        File.Move(temporary, fullDestination)
        completed <- true
    finally
        if not completed && File.Exists(temporary) then
            File.Delete(temporary)

let findPathCaseInsensitive dirPath (filename: string) =
    let files =
        Directory.GetFiles(dirPath)
        |> Array.filter
            (fun f -> Path.GetFileName(f).ToLower() = filename.ToLower())
    match files.Length with
    | 1 -> Some files.[0]
    | 0 -> None
    | _ ->
        failwithf "Multiple files found when looking for %s in %s (case insensitive)"
                  filename dirPath
