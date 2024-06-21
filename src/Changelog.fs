module EasyBuild.ChangelogGen.Changelog

open System
open System.Text.RegularExpressions
open Semver
open FsToolkit.ErrorHandling

type Metadata = { LastCommitReleased: string }

type VersionEntry =
    {
        Version: SemVersion
        Date: DateOnly option
        Body: string
    }

type Changelog =
    {
        Title: string option
        Description: string option
        Metadata: string option
        Unreleased: VersionEntry option
        Versions: VersionEntry list
    }

[<RequireQualifiedAccess>]
type Symbols =
    | Title of title: string
    | RawText of body: string
    | SectionHeader of title: string * version: SemVersion * date: DateOnly option
    | SubSection of tag: string
    | SubSubSection of tag: string
    | ListItem of content: string

[<RequireQualifiedAccess>]
module Lexer =

    [<return: Struct>]
    let private (|Match|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            ValueSome m
        else
            ValueNone

    [<return: Struct>]
    let private (|Title|_|) (input: string) =
        if input.StartsWith("# ") then
            input.Substring(1).Trim() |> ValueSome
        else
            ValueNone

    [<return: Struct>]
    let private (|Semver|_|) (input: string) =
        match input with
        | Match "\\[?v?(?<version>[\\w\\d.-]+\\.[\\w\\d.-]+[a-zA-Z0-9])\\]?" m ->
            let version = m.Groups.["version"].Value

            match SemVersion.TryParse(version, SemVersionStyles.Strict) with
            | true, version -> ValueSome version
            | _ -> ValueNone
        | _ -> ValueNone

    let internal tryParseSemver (input: string) =
        let m = Regex.Match(input, "\\[?v?(?<version>[\\w\\d.-]+\\.[\\w\\d.-]+[a-zA-Z0-9])\\]?")
        if m.Success then
            let version = m.Groups.["version"].Value
            match SemVersion.TryParse(version, SemVersionStyles.Strict) with
            | true, version -> Ok version
            | _ ->
                $"""Invalid version format:

%s{input}

Please make sure the version is conforming to the SemVer 2.0 spec.
For more information, visit https://semver.org/spec/v2.0.0.html."""
                |> Error

        else
            $"""Could not find a valid version in the line:

%s{input}

Please make sure the version is conforming to the SemVer 2.0 spec.
For more information, visit https://semver.org/spec/v2.0.0.html."""
            |> Error

    // [<return: Struct>]
    // let private (|Date|_|) (input: string) =
    //     match input with
    //     | Match "(\\d{4}-\\d{2}-\\d{2})" m ->
    //         match DateOnly.TryParse(m.Groups.[0].Value) with
    //         | true, date -> ValueSome date
    //         | _ -> ValueNone
    //     | _ -> ValueNone

    let internal tryParseDate (input: string) =
        let m = Regex.Match(input, "(\\d{4}-\\d{2}-\\d{2})")
        if m.Success then
            let date = m.Groups.[0].Value
            match DateOnly.TryParse(date) with
            | true, date -> Ok (Some date)
            | _ ->
                $"""Invalid date format:

%s{input}

Please make sure the date is conforming to the ISO 8601 format (YYYY-MM-DD)."""
                |> Error

        else
            Ok None



    // [<return: Struct>]
    let internal tryParseVersion (input: string) =
        match input with
        | Match "^## [^#]" _ ->
            result {
                let! version = tryParseSemver input
                let! date = tryParseDate input
                let title = input.Substring(3).Trim()

                return Symbols.SectionHeader(title, version, date)
            }
            |> Some

        | _ -> None

// [<return: Struct>]
// let private (|SubSection|_|) (input: string) =
//     match input with
//     | Match "^### ?[^#]" _ -> input.Substring(3).Trim() |> ValueSome
//     | _ -> ValueNone

// [<return: Struct>]
// let private (|SubSubSection|_|) (input: string) =
//     match input with
//     | Match "^#### ?[^#]" _ -> input.Substring(4).Trim() |> ValueSome
//     | _ -> ValueNone

// [<return: Struct>]
// let private (|ListItem|_|) (input: string) =
//     match input with
//     | Match "^[*-]" _ -> input.Substring(1).Trim() |> ValueSome
//     | _ -> ValueNone

// let toSymbols (lines: string list) =
//     lines
//     |> List.map (
//         function
//         | Title title -> Symbols.Title title
//         | Version(title, version, date) -> Symbols.SectionHeader(title, version, date)
//         | SubSection tag -> Symbols.SubSection tag
//         | SubSubSection tag -> Symbols.SubSubSection tag
//         | ListItem content -> Symbols.ListItem content
//         | rawText -> Symbols.RawText(rawText.TrimEnd())
//     )

    let internal tryParseTitle (input: string) =
        if input.StartsWith("# ") then
            input.Substring(1).Trim() |> Some
        else
            None



    let rec toSymbols (lines : string list) acc =
        match lines with
        | [] -> Ok acc
        | x :: xs ->
            match tryParseTitle x with
            | Some title -> toSymbols xs (Symbols.Title title :: acc)
            | None ->
                match tryParseVersion x with
                | Some version ->
                    match version with
                    | Ok version -> toSymbols xs (version :: acc)
                    | Error err -> Error err
                | None -> toSymbols xs (Symbols.RawText x :: acc)




let parse (changelogContent: string) =
    let lines = changelogContent.Replace("\r\n", "\n").Split("\n") |> Array.toList
    ()
