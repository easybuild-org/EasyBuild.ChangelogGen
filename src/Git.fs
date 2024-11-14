[<RequireQualifiedAccess>]
module Git

open SimpleExec
open BlackFox.CommandLine
open Thoth.Json.Core
open Thoth.Json.Newtonsoft

let getHeadBranchName () =
    let struct (standardOutput, _) =
        Command.ReadAsync(
            "git",
            CmdLine.empty
            |> CmdLine.appendRaw "rev-parse"
            |> CmdLine.appendPrefix "--abbrev-ref" "HEAD"
            |> CmdLine.toString
        )
        |> Async.AwaitTask
        |> Async.RunSynchronously

    standardOutput.Trim()

let isDirty () =
    let struct (standardOutput, _) =
        Command.ReadAsync(
            "git",
            CmdLine.empty
            |> CmdLine.appendRaw "status"
            |> CmdLine.appendRaw "--porcelain"
            |> CmdLine.toString
        )
        |> Async.AwaitTask
        |> Async.RunSynchronously

    standardOutput.Trim().Length > 0

type Commit =
    {
        Hash: string
        AbbrevHash: string
        Author: string
        ShortMessage: string
        RawBody: string
    }

    static member Decoder: Decoder<Commit> =
        Decode.object (fun get ->
            {
                Hash = get.Required.Field "hash" Decode.string
                AbbrevHash = get.Required.Field "abbrev_hash" Decode.string
                Author = get.Required.Field "author" Decode.string
                ShortMessage = get.Required.Field "short_message" Decode.string
                RawBody = get.Required.Field "long_message" Decode.string
            }
        )

[<RequireQualifiedAccess>]
type GetCommitsFilter =
    | All
    | From of string

// |> CmdLine.appendRaw "--format=\"{\n  \\\"hash\\\": \\\"%H\\\",\\\"abbrev_hash\\\": \\\"%h\\\",\n  \\\"author\\\": \\\"%an\\\",\n  \\\"short_message\\\": \\\"%s\\\",\n  \\\"long_message\\\": \\\"%B\\\"\n}\""

let readCommit (sha1: string) =
    let struct (commitStdout, _) =
        Command.ReadAsync(
            "git",
            CmdLine.empty
            |> CmdLine.appendRaw "--no-pager"
            |> CmdLine.appendRaw "show"
            |> CmdLine.appendRaw
                """--format="{
  \"hash\": \"%H\",
  \"abbrev_hash\": \"%h\",
  \"author\": \"%an\",
  \"short_message\": \"%s\",
  \"long_message\": \"%B\"
}"
                """
            |> CmdLine.appendRaw "-s" // suppress diff output
            |> CmdLine.appendRaw sha1
            |> CmdLine.toString
        )
        |> Async.AwaitTask
        |> Async.RunSynchronously

    match Decode.fromString Commit.Decoder commitStdout with
    | Ok x -> x
    | Error e -> failwith e

let getCommits (filter: GetCommitsFilter) =
    let commitFilter =
        match filter with
        | GetCommitsFilter.All -> "HEAD"
        | GetCommitsFilter.From sha1 -> $"{sha1}..HEAD"

    let struct (shaStdout, _) =
        Command.ReadAsync(
            "git",
            CmdLine.empty
            |> CmdLine.appendRaw "rev-list"
            |> CmdLine.appendRaw commitFilter
            |> CmdLine.toString
        )
        |> Async.AwaitTask
        |> Async.RunSynchronously

    shaStdout.Split('\n')
    |> Array.filter (fun x -> x.Length > 0)
    |> Array.map readCommit
    |> Array.toList
