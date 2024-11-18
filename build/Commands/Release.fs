module EasyBuild.Commands.Release

open Spectre.Console.Cli
open SimpleExec
open EasyBuild.Workspace
open System
open System.IO
open BlackFox.CommandLine
open EasyBuild.Tools.Dotnet
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
                |> CmdLine.appendRaw "--"
                |> CmdLine.appendSeq context.Remaining.Raw
                |> CmdLine.toString,
                workingDirectory = Workspace.``.``
            )
            |> Async.AwaitTask
            |> Async.RunSynchronously

        let nupkgPath = Nuget.pack Workspace.src.``.``

        let nugetKey =
            match Environment.GetEnvironmentVariable("NUGET_KEY") with
            | null -> failwith "NUGET_KEY environment variable not set."
            | key -> key

        Nuget.push (nupkgPath, nugetKey)

        Command.Run("git", "add .")

        Command.Run(
            "git",
            CmdLine.empty
            |> CmdLine.appendRaw "commit"
            |> CmdLine.appendPrefix "-m" $"chore: release %s{newVersion}"
            |> CmdLine.toString
        )

        Command.Run("git", "push")

        0
