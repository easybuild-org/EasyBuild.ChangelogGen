[<RequireQualifiedAccess>]
module Git

open System
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

[<RequireQualifiedAccess>]
type GetCommitsFilter =
    | All
    | From of string

let readCommit (sha1: string) =
    let delimiter = "----PARSER_DELIMITER----"

    let gitFormat =
        [
            "%H" // commit hash
            "%h" // abbreviated commit hash
            "%an" // author name
            "%s" // subject
            "%B" // body / long message
            "-" // We need an extra item otherwise, the body will have an additional new line at the end
        ]
        |> String.concat delimiter

    let args =
        CmdLine.empty
        |> CmdLine.appendRaw "--no-pager"
        |> CmdLine.appendRaw "show"
        |> CmdLine.appendRaw $"--format={gitFormat}"
        |> CmdLine.appendRaw "-s" // suppress diff output
        |> CmdLine.appendRaw sha1
        |> CmdLine.toString

    let struct (commitStdout, _) =
        Command.ReadAsync("git", args) |> Async.AwaitTask |> Async.RunSynchronously

    let commitStdout = commitStdout.Split(delimiter, StringSplitOptions.None)

    if commitStdout.Length <> 6 then
        failwith
            $"""Failed to read commit {sha1}, unexpected output from git show

Output was:
{commitStdout}
"""

    {
        Hash = commitStdout[0]
        AbbrevHash = commitStdout[1]
        Author = commitStdout[2]
        ShortMessage = commitStdout[3]
        RawBody = commitStdout[4]
    }

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

type Remote =
    {
        Hostname: string
        Owner: string
        Repository: string
    }

let private stripSuffix (suffix: string) (str: string) =
    if str.EndsWith(suffix) then
        str.Substring(0, str.Length - suffix.Length)
    else
        str

let private stripPrefix (prefix: string) (str: string) =
    if str.StartsWith(prefix) then
        str.Substring(prefix.Length)
    else
        str

// Url needs to be in the format:
// https://hostname/owner/repo.git
let tryGetRemoteFromUrl (url: string) =
    let normalizedUrl = url |> stripSuffix ".git"

    match Uri.TryCreate(normalizedUrl, UriKind.Absolute) with
    | true, uri ->
        let segments =
            uri.Segments
            |> Seq.map _.Trim('/')
            |> Seq.filter (String.IsNullOrEmpty >> not)
            |> Seq.toList

        if segments.Length < 2 then
            None
        else
            let owner = segments.[segments.Length - 2]
            let repo = segments.[segments.Length - 1]

            Some
                {
                    Hostname = uri.Host
                    Owner = owner.Trim('/')
                    Repository = repo.Trim('/')
                }
    | false, _ -> None

let tryGetRemoteFromSSH (url: string) =
    // Naive way to check the format
    if url.Contains("@") && url.Contains(":") && url.Contains("/") then
        let segments =
            // Remove the .git extension and split the url
            url |> stripPrefix "git@" |> stripSuffix ".git" |> _.Split(':') |> Seq.toList

        match segments with
        | hostname :: owner_repo :: _ ->
            let segments = owner_repo.Split('/') |> Array.rev |> Array.toList

            match segments with
            | repo :: owner :: _ ->
                Some
                    {
                        Hostname = hostname
                        Owner = owner
                        Repository = repo
                    }
            | _ -> None
        | _ -> None
    else
        None

let tryFindRemote () =
    let struct (remoteStdout, _) =
        try
            Command.ReadAsync(
                "git",
                CmdLine.empty
                |> CmdLine.appendRaw "config"
                |> CmdLine.appendPrefix "--get" "remote.origin.url"
                |> CmdLine.toString
            )
            |> Async.AwaitTask
            |> Async.RunSynchronously
        with
        // If we don't have a remote configured the Git command returns exitCode 1
        // making the invocation throw an exception
        | _ ->
            struct ("", "")

    let remoteUrl = remoteStdout.Trim()

    match tryGetRemoteFromUrl remoteUrl with
    | Some remote -> Ok remote
    | None ->
        match tryGetRemoteFromSSH remoteUrl with
        | Some remote -> Ok remote
        | None ->
            Error
                """Could not resolve the remote repository.

Automatic detection expects URL returned by `git config --get remote.origin.url` to be of the form 'https://hostname/owner/repo.git' or 'git@hostname:owner/repo.git'.

You can use the --github-repo option to specify the repository manually."""
