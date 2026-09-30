// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later
// (c) 2023 David Koňařík

/// Date bitmaps, date parsing and GVD (timetable year) bounds.
module JrUtil.DateUtils

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

type DateBitmap(interval: DateInterval, bits: BitArray) =
    do
        if bits.Length <> interval.Length then
            failwith "DateBitmap day/bit count mismatch"

    member this.Interval = interval
    member this.Bits = bits

    member this.Not() =
        let bitsCopy = bits.Clone() :?> BitArray
        bitsCopy.Not() |> ignore
        DateBitmap(interval, bitsCopy)
    member this.And(other: DateBitmap) =
        assert (interval = other.Interval)
        let bitsCopy = bits.Clone() :?> BitArray
        bitsCopy.And(other.Bits) |> ignore
        DateBitmap(interval, bitsCopy)
    member this.ExtendTo(resInterval: DateInterval, value: bool) =
        assert resInterval.Contains(interval)
        let extended = BitArray(resInterval.Length)
        extended.SetAll(value)
        let offset = Period.DaysBetween(resInterval.Start, interval.Start)
        for i in 0..(bits.Length - 1) do
            extended.[offset + i] <- bits.[i]
        DateBitmap(resInterval, extended)
    member this.HasAnySet() = this.Bits.HasAnySet()

let tryParseDate (format: string) (str: string) =
    let success, dt = DateTime.TryParseExact(
        str, format, CultureInfo.InvariantCulture, DateTimeStyles.AssumeLocal)
    if success then Some <| LocalDate.FromDateTime(dt)
    else None
let parseDate format str =
    tryParseDate format str |> Option.get

let rec dateRange (startDate: LocalDate) (endDate: LocalDate) =
    // Create a list of Dates containing all days between startDate
    // and endDate *inclusive*
    if startDate <= endDate
    then startDate :: (dateRange (startDate.PlusDays(1)) endDate)
    else []

let rec dateTimeRange (startDate: DateTime) (endDate: DateTime)  =
    // Create a list of DateTime objects containing all days between
    // startDate and endDate *inclusive*
    if startDate <= endDate
    then startDate :: (dateTimeRange (startDate.AddDays(1.0)) endDate)
    else []

let dateToday () =
    LocalDate.FromDateTime(DateTime.Today)

/// The first day of the Czech timetable year (GVD) valid from December of `year`:
/// the second Sunday of December.
let gvdSecondSunday year =
    let first = LocalDate(year, 12, 1)
    let offset = (int IsoDayOfWeek.Sunday - int first.DayOfWeek + 7) % 7 + 7
    first.PlusDays(offset)

/// First and last day of GVD `year`, e.g. 2026 = 2025-12-14 .. 2026-12-12.
let gvdBounds year =
    gvdSecondSunday (year - 1), (gvdSecondSunday year).PlusDays(-1)
