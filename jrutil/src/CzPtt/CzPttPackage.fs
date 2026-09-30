// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

/// CZPTT to production package: converts the messages, hands the in-memory
/// feed and spooled source metadata to the package writer and activates the
/// package atomically.
module JrUtil.CzPttPackage

open System
open System.Collections.Generic
open System.IO
open System.Text.Json

open JrUtil.Serving

type Options = {
    catalog: CzPttModel.CatalogSnapshot
    conversion: CzPttModel.ConversionOptions
    sr70Path: string option
    osmPath: string option
    osmAliasesPath: string option
    diagnosticsOutput: string option
    diagnosticTraces: bool
}

let private json (value: obj) =
    use document = JsonDocument.Parse(JsonSerializer.Serialize(value, JsonSerializerOptions(WriteIndented = true)))
    document.RootElement.Clone()

/// `progress phase state` reports "started"/"completed" for each phase.
let write (options: Options) (inputPath: string) (outputPath: string) (progress: string -> string -> unit) =
    let finalOutput = Path.GetFullPath(outputPath)
    if Directory.Exists(finalOutput) || File.Exists(finalOutput) then
        invalidArg "outputPath" "Output path must not exist"
    let parent = Path.GetDirectoryName(finalOutput)
    Directory.CreateDirectory(parent) |> ignore
    let unique = Guid.NewGuid().ToString("N")
    // Private scratch: the merger spill and the typed source-metadata spools.
    let scratch = Path.Combine(parent, $".{Path.GetFileName(finalOutput)}.{unique}.czptt")
    let production = Path.Combine(parent, $".{Path.GetFileName(finalOutput)}.{unique}.tmp")
    Directory.CreateDirectory(scratch) |> ignore
    try
        let result =
            CzPttBundle.convert
                CzPttBundle.SpillBacked options.catalog options.conversion inputPath scratch
                options.sr70Path options.osmPath options.osmAliasesPath progress
        progress "prepare-package-input" "started"
        let feed = result.feed |> Gtfs.deduplicateCalendar |> Gtfs.fillStandardRequiredFields
        let standard, czech = Gtfs.feedTables feed
        let asTables entries =
            entries |> Seq.map (fun (name, header, rows) -> name, CompilerOutput.memoryTable header rows) |> CompilerOutput.tables
        let diagnostics = Dictionary<string, obj>()
        diagnostics.["schema_version"] <- box 1
        diagnostics.["operational_points"] <-
            box (match options.conversion.operationalPointMode with
                 | CzPttModel.Gtfs -> "gtfs"
                 | CzPttModel.Sidecar -> "sidecar")
        diagnostics.["accepted_pa_count"] <- box result.acceptedPaIds.Length
        diagnostics.["rejected_journeys"] <- box result.rejectedJourneys
        diagnostics.["cancelled_pa_ids"] <- box result.cancelledPaIds
        diagnostics.["sidecar_boundary_approximations"] <- box result.sidecarBoundaryApproximations
        diagnostics.["line_boundary_adjustments"] <- box result.boundaryAdjustments
        diagnostics.["ids_diagnostics"] <- box result.idsDiagnostics
        diagnostics.["merge_diagnostics"] <- box result.mergeDiagnostics
        diagnostics.["coordinate_diagnostics"] <- box result.coordinateDiagnostics
        let input: CompilerOutput.Output = {
            gtfs = asTables standard
            czech = asTables czech
            mappings = CompilerOutput.noTables
            reports = CompilerOutput.noTables
            sidecars =
                Directory.EnumerateFiles(scratch, "*.parquet")
                |> Seq.map (fun path -> Path.GetFileNameWithoutExtension(path), CompilerOutput.parquetFileTable path)
                |> CompilerOutput.tables
            manifest = json (dict [ "schema_version", box 1; "source_format", box "czptt" ])
            diagnostics = Some (json diagnostics)
            basePackage = None
            diagnosticFiles = Dictionary<string, string>() :> IReadOnlyDictionary<_, _>
        }
        progress "prepare-package-input" "completed"
        progress "write-package" "started"
        PackageWriter.writePackage Map.empty None (fun _ _ -> ()) input production
        options.diagnosticsOutput
        |> Option.iter (fun path -> PackageWriter.writeDiagnosticArtifact input path options.diagnosticTraces)
        Directory.Move(production, finalOutput)
        progress "write-package" "completed"
        result
    finally
        if Directory.Exists(scratch) then Directory.Delete(scratch, true)
        if Directory.Exists(production) then Directory.Delete(production, true)
