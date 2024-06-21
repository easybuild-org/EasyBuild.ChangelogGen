namespace EasyBuild.ChangelogGen.Commands

open Spectre.Console
open Spectre.Console.Cli
open System.ComponentModel
open System.IO
open Thoth.Json.Newtonsoft

type VersionSettings() =
    inherit CommandSettings()

type VersionCommand() =
    inherit Command<VersionSettings>()
    interface ICommandLimiter<VersionSettings>

    override __.Execute(context, settings) =
        printfn "Version command"
        0
