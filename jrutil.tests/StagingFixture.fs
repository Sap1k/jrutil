// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.
namespace JrUtil.Tests

open System
open System.Collections.Generic
open System.IO
open System.Text.Json

open JrUtil.Serving

/// Test fixtures describe a compiler hand-off as a directory tree
/// (gtfs-intermediate/, extensions/, mappings/, reports/, *.parquet,
/// manifest.json, diagnostics.json); production compilers build it in memory.
module StagingFixture =
    /// Write a feed's standard GTFS tables as CSV files.
    let writeFeed (root: string) (feed: JrUtil.GtfsModel.GtfsFeed) =
        Directory.CreateDirectory(root) |> ignore
        let standard, _ = JrUtil.Gtfs.feedTables feed
        for name, header, rows in standard do
            use writer = new StreamWriter(Path.Combine(root, name))
            CompilerOutput.writeCsv writer (CompilerOutput.memoryTable header rows)

    let output (legacy: string) : CompilerOutput.Output =
        let files (directory: string) (pattern: string) =
            let root = Path.Combine(legacy, directory)
            if Directory.Exists(root) then
                Directory.EnumerateFiles(root, pattern) |> Seq.map (fun path -> Path.GetFileName(path), CompilerOutput.csvFileTable path) |> CompilerOutput.tables
            else CompilerOutput.noTables
        let sidecars =
            Directory.EnumerateFiles(legacy, "*.parquet")
            |> Seq.map (fun path -> Path.GetFileNameWithoutExtension(path), CompilerOutput.parquetFileTable path)
            |> CompilerOutput.tables
        let json (path: string) =
            use document = JsonDocument.Parse(File.ReadAllText(path))
            document.RootElement.Clone()
        let manifestPath = Path.Combine(legacy, "manifest.json")
        let diagnosticsPath = Path.Combine(legacy, "diagnostics.json")
        let basePackage =
            let reference = Path.Combine(legacy, "base-package.path")
            if File.Exists(reference) then
                let package = File.ReadAllText(reference).Trim()
                if Directory.Exists(package) then Some package else None
            else
                let copied = Path.Combine(legacy, "base-evidence")
                if File.Exists(Path.Combine(copied, "manifest.json")) then Some copied else None
        let diagnosticFiles = Dictionary<string, string>(StringComparer.Ordinal)
        for directory, target in [ "policies", "inputs/policies"; "source-descriptors", "inputs/source-descriptors" ] do
            let root = Path.Combine(legacy, directory)
            if Directory.Exists(root) then
                for path in Directory.EnumerateFiles(root, "*", SearchOption.AllDirectories) do
                    diagnosticFiles.[target + "/" + Path.GetRelativePath(root, path).Replace('\\', '/')] <- path
        let manifest = if File.Exists(manifestPath) then json manifestPath else JsonDocument.Parse("{}").RootElement.Clone()
        let feed =
            match manifest.TryGetProperty("source_format") with
            | true, value when value.ValueKind = JsonValueKind.String && value.GetString() = "czptt" -> "czptt"
            | _ -> "jdf"
        { feed = feed
          gtfs = files "gtfs-intermediate" "*.txt"
          czech = files "extensions" "*.txt"
          mappings = files "mappings" "*.csv"
          reports = files "reports" "*.csv"
          sidecars = sidecars
          manifest = manifest
          diagnostics = if File.Exists(diagnosticsPath) then Some (json diagnosticsPath) else None
          basePackage = basePackage
          diagnosticFiles = diagnosticFiles :> IReadOnlyDictionary<_, _> }

    /// Write a production package from a fixture tree.
    let finalize compiled nativeSummaries progress (legacy: string) outputPath =
        PackageWriter.writePackage compiled nativeSummaries progress (output legacy) outputPath
