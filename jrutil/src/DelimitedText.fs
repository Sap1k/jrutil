// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later

/// Comma-separated records with the semantics of
/// `Microsoft.VisualBasic.FileIO.TextFieldParser` configured for quoted fields
/// without trimming, minus its per-field regular expressions and buffers.
/// Blank and whitespace-only lines are skipped (also inside quoted fields),
/// whitespace may surround a quoted field, line ends inside quoted fields are
/// kept verbatim, and anything else after a closing quote is malformed.
module JrUtil.DelimitedText

open System
open System.IO
open System.Text

/// TextFieldParser's default whitespace set; it never contains CR or LF.
let private isParserWhitespace (character: char) =
    match int character with
    | 0x09 | 0x0B | 0x0C | 0x20 | 0x85 | 0xA0 | 0x1680
    | 0x2028 | 0x2029 | 0x3000 | 0xFEFF -> true
    | code -> code >= 0x2000 && code <= 0x200B

let private isLineEnd (character: char) = character = '\r' || character = '\n'

/// Physical lines including their terminator (CR, LF or CRLF).
type private LineReader(reader: TextReader) =
    let buffer = Array.zeroCreate<char> (64 * 1024)
    let mutable length = 0
    let mutable position = 0
    let pending = StringBuilder()
    let fill () =
        length <- reader.Read(buffer, 0, buffer.Length)
        position <- 0
        length > 0

    member _.ReadLine() : string =
        pending.Clear() |> ignore
        let mutable result: string = null
        let mutable finished = false
        while not finished do
            if position >= length && not (fill ()) then
                finished <- true
                if pending.Length > 0 then result <- pending.ToString()
            else
                let start = position
                let mutable index = position
                while index < length && not (isLineEnd buffer.[index]) do
                    index <- index + 1
                if index = length then
                    pending.Append(buffer, start, length - start) |> ignore
                    position <- length
                else
                    let crlfInBuffer =
                        buffer.[index] = '\r' && index + 1 < length && buffer.[index + 1] = '\n'
                    let terminatorEnd = if crlfInBuffer then index + 2 else index + 1
                    position <- terminatorEnd
                    if pending.Length = 0 && (buffer.[index] = '\n' || crlfInBuffer) then
                        result <- String(buffer, start, terminatorEnd - start)
                    else
                        pending.Append(buffer, start, terminatorEnd - start) |> ignore
                        // A CR at the end of the buffer may be the first half of CRLF.
                        if buffer.[index] = '\r' && not crlfInBuffer
                           && position >= length && fill () && buffer.[0] = '\n' then
                            pending.Append('\n') |> ignore
                            position <- 1
                        result <- pending.ToString()
                    finished <- true
        result

let private isBlank (line: string) =
    let mutable blank = true
    let mutable index = 0
    while blank && index < line.Length do
        blank <- Char.IsWhiteSpace(line.[index])
        index <- index + 1
    blank

/// Index where the line terminator starts, or the length without one.
let private endOfLineIndex (line: string) =
    let length = line.Length
    if length = 1 then length
    elif isLineEnd line.[length - 2] then length - 2
    elif isLineEnd line.[length - 1] then length - 1
    else length

/// Streams records of comma-separated text. Malformed quoting raises
/// InvalidDataException naming `source`.
type CsvRecordReader(reader: TextReader, source: string) =
    let lines = LineReader(reader)
    let field = StringBuilder()
    let fields = ResizeArray<string>()

    let nextDataLine () =
        let mutable line = lines.ReadLine()
        while not (isNull line) && isBlank line do
            line <- lines.ReadLine()
        line

    let malformed () =
        raise (InvalidDataException($"{source} contains a malformed quoted field"))

    /// Read a quoted field starting after its opening quote. Returns the index
    /// of the closing quote and the length of the quote, padding and delimiter;
    /// None when the line ended inside the field.
    let buildQuoted (line: string) (startAt: int) =
        let length = line.Length
        let mutable index = startAt
        let mutable result = ValueNone
        let mutable finished = false
        while not finished && index < length do
            let character = line.[index]
            if character <> '"' then
                field.Append(character) |> ignore
                index <- index + 1
            elif index + 1 = length then
                result <- ValueSome struct(index + 1, 1)
                finished <- true
            elif line.[index + 1] = '"' then
                field.Append('"') |> ignore
                index <- index + 2
            else
                let mutable delimiter = index + 1
                while delimiter < length && line.[delimiter] <> ',' && not (isLineEnd line.[delimiter]) do
                    delimiter <- delimiter + 1
                let limit = if delimiter < length then delimiter - 1 else length - 1
                for padding in index + 1 .. limit do
                    if not (isParserWhitespace line.[padding]) then malformed ()
                let delimiterLength = if delimiter < length then 1 else 0
                result <- ValueSome struct(index, 1 + limit - index + delimiterLength)
                finished <- true
        result

    /// The next record, or null at the end of the input.
    member _.Read() : string array =
        let mutable line = nextDataLine ()
        if isNull line then null else
        fields.Clear()
        let mutable lineEnd = endOfLineIndex line
        let mutable index = 0
        let mutable reading = true
        while reading && index <= lineEnd do
            let mutable quoteStart = index
            while quoteStart < line.Length && isParserWhitespace line.[quoteStart] do
                quoteStart <- quoteStart + 1
            if quoteStart < line.Length && line.[quoteStart] = '"' then
                field.Clear() |> ignore
                let mutable built = buildQuoted line (quoteStart + 1)
                while built.IsNone do
                    let continuedFrom = line.Length
                    let next = nextDataLine ()
                    if isNull next then malformed ()
                    line <- line + next
                    lineEnd <- endOfLineIndex line
                    built <- buildQuoted line continuedFrom
                let struct(closing, consumed) = built.Value
                fields.Add(field.ToString())
                index <- closing + consumed
            else
                let delimiter = line.IndexOf(',', index)
                if delimiter >= 0 then
                    fields.Add(line.Substring(index, delimiter - index))
                    index <- delimiter + 1
                else
                    fields.Add(line.Substring(index).TrimEnd('\r', '\n'))
                    reading <- false
        fields.ToArray()

    /// All remaining records.
    member this.Records() = seq {
        let mutable record = this.Read()
        while not (isNull record) do
            yield record
            record <- this.Read()
    }
