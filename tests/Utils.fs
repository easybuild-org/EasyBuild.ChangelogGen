module Tests.Utils

open Expecto

module Expect =

    let equal actual expected = Expect.equal actual expected ""

    let notEqual actual expected = Expect.notEqual actual expected ""

    let isNotEmpty actual = Expect.isNotEmpty actual ""

type Git.Commit with

    static member Create
        (hash: string, shortMessage: string, ?longMessage: string, ?author: string)
        : Git.Commit
        =
        let author = defaultArg author "Kaladin Stormblessed"
        let longMessage = defaultArg longMessage (shortMessage + "\n")

        {
            Hash = hash
            AbbrevHash = hash.Substring(0, 7)
            Author = author
            ShortMessage = shortMessage
            RawBody = longMessage
        }

open VerifyTests
open VerifyExpecto
open DiffEngine
open Workspace
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

let mutable diffToolRegistered = false

let registerDiffTool () =
    if not diffToolRegistered then
        let launchArguments =
            LaunchArguments(
                Left = fun temp target -> $"-n --diff \"{target}\" \"{temp}\""
                , Right = fun temp target -> $"-n --diff \"{temp}\" \"{target}\""
            )

        DiffTools.AddToolBasedOn(
            DiffTool.VisualStudioCode,
            "VisualStudioCodeNewWindow",
            launchArguments = launchArguments
        )
        |> ignore

        diffToolRegistered <- true

let inline verify (name: string) (value: string) =
    // registerDiffTool()

    let settings = VerifySettings()
    settings.UseDirectory(Workspace.``..``.VerifyTests.``.``)
    Verifier.Verify(name, value, settings, "dwdw").ToTask()

type Verify =

    static member Markdown
        (
            name: string,
            value: string,
            [<CallerFilePath; Optional; DefaultParameterValue("")>] callerFilePath: string
        )

        =
        let settings = VerifySettings()
        settings.UseDirectory(Workspace.``..``.VerifyTests.``.``)

        Verifier.Verify(name, value, "md", settings, callerFilePath).ToTask()

type TestHelper =
    static member testMarkdown
        (
            name: string,
            func: unit -> string,
            [<CallerFilePath; Optional; DefaultParameterValue("")>] callerFilePath: string
        )
        =
        testTask name { do! Verify.Markdown(name, func (), callerFilePath) }
