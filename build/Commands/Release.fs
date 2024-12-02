module EasyBuild.Commands.Release

open Spectre.Console.Cli
open SimpleExec
open EasyBuild.Workspace
open System
open System.IO
open BlackFox.CommandLine
open EasyBuild.Tools.DotNet
open EasyBuild.Tools.Git
open EasyBuild.Commands.Test

type ReleaseSettings() =
    inherit CommandSettings()

type ReleaseCommand() =
    inherit Command<ReleaseSettings>()
    interface ICommandLimiter<ReleaseSettings>

    override __.Execute(context, settings) =
        TestCommand().Execute(context, TestSettings()) |> ignore

        // Clean up the src/bin folder
        if Directory.Exists VirtualWorkspace.src.bin.``.`` then
            Directory.Delete(VirtualWorkspace.src.bin.``.``, true)

        let (struct (newVersion, _)) =
            Command.ReadAsync(
                "dotnet",
                CmdLine.empty
                |> CmdLine.appendRaw "run"
                |> CmdLine.appendPrefix "--project" Workspace.src.``EasyBuild.ChangelogGen.fsproj``
                |> CmdLine.appendPrefix "--configuration" "Release"
                |> CmdLine.appendRaw "--"
                |> CmdLine.appendSeq context.Remaining.Raw
                |> CmdLine.toString,
                workingDirectory = Workspace.``.``
            )
            |> Async.AwaitTask
            |> Async.RunSynchronously

        let nupkgPath = DotNet.pack Workspace.src.``.``

        DotNet.nugetPush nupkgPath

        Git.addAll ()
        Git.commitRelease newVersion
        Git.push ()

        0
