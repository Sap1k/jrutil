// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

namespace JrUtil.Serving

open System
open System.Security.Cryptography
open System.Text

module Identity =
    let sha256 (value: string) = JrUtil.Hashing.sha256Text value

    /// RFC 3986 unreserved bytes are left readable. Everything else is encoded
    /// from UTF-8 bytes, making composite provider keys reversible and stable.
    let encodeComponent (value: string) =
        let unreserved value =
            (value >= byte 'a' && value <= byte 'z')
            || (value >= byte 'A' && value <= byte 'Z')
            || (value >= byte '0' && value <= byte '9')
            || value = byte '-' || value = byte '.' || value = byte '_' || value = byte '~'
        let mutable index = 0
        while index < value.Length && int value.[index] < 128 && unreserved (byte value.[index]) do
            index <- index + 1
        if index = value.Length then value
        else
            let builder = StringBuilder(value.Length + 16)
            let hex = "0123456789ABCDEF"
            for value in Encoding.UTF8.GetBytes(value) do
                if unreserved value then builder.Append(char value) |> ignore
                else
                    builder.Append('%').Append(hex.[int value >>> 4]).Append(hex.[int value &&& 15]) |> ignore
            builder.ToString()

    // Composite keys use '/' as their only structural delimiter. Colons are a
    // normal part of public GTFS/JDF identifiers and are safe inside a path
    // segment, so keep them readable while still escaping '/' and arbitrary
    // provider data.
    let private encodePathComponent (value: string) =
        encodeComponent value |> fun encoded -> encoded.Replace("%3A", ":")

    let compositeKey (components: string seq) =
        components |> Seq.map encodePathComponent |> String.concat "/"

    /// Route stop key. JDF route stop numbers are unique only within one merged
    /// route version, and one output route can hold several versions.
    let routeStopKey route (version: string) stop =
        if String.IsNullOrEmpty version then compositeKey [ route; stop ]
        else compositeKey [ route; version; stop ]

    let canonicalFields (fields: (string * string) seq) =
        fields
        |> Seq.sortBy fst
        |> Seq.map (fun (name, value) -> encodeComponent name + "=" + encodeComponent value)
        |> String.concat "&"

    let private digest kind fields =
        (sha256 ("jrutil-serving-v2\n" + kind + "\n" + canonicalFields fields)).Substring(0, 32)

    let bindingId kind fields = "v1:" + kind + ":" + digest kind fields

    /// Feed-prefixed generated identifier (`<feed>:<kind>:<digest>`).
    let feedId (feed: string) kind fields = feed + ":" + kind + ":" + digest kind fields
