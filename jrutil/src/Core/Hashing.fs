// This file is part of JrUtil and is licenced under the GNU AGPLv3 or later.

/// Lower-case hex SHA-256 digests. Identifier schemes that truncate or
/// upper-case a digest keep their own formatting next to the scheme.
module JrUtil.Hashing

open System
open System.IO
open System.Security.Cryptography
open System.Text

let sha256Bytes (bytes: byte array) =
    Convert.ToHexString(SHA256.HashData(bytes)).ToLowerInvariant()

let sha256Text (value: string) =
    value |> Encoding.UTF8.GetBytes |> sha256Bytes

let sha256Stream (stream: Stream) =
    Convert.ToHexString(SHA256.HashData(stream)).ToLowerInvariant()

let sha256File (path: string) =
    use stream = File.OpenRead(path)
    sha256Stream stream
