module EasyBuild.Main

open Spectre.Console.Cli
open EasyBuild.Commands.Test
open EasyBuild.Commands.Release
open SimpleExec

[<EntryPoint>]
let main args =

    Command.Run("dotnet", "husky install")

    let app = CommandApp()

    app.Configure(fun config ->
        config.Settings.ApplicationName <- "./build.sh"

        config
            .AddCommand<TestCommand>("test")
            .WithDescription("Run the tests")
            .WithExample("test")
            .WithExample("test --watch")
        |> ignore

        config
            .AddCommand<ReleaseCommand>("release")
            .WithDescription(
                "Package a new version of the library and publish it to NuGet. You can use `-- --help` to see all the available options."
            )
            .WithExample("release -- --help [Show all available options]")
            .WithExample("release -- --major")
        |> ignore

    )

    app.Run(args)
