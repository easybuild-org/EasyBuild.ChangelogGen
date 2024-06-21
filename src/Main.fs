module EasyBuild.ChangelogGen.Main

open Spectre.Console.Cli
open EasyBuild.ChangelogGen.Commands

[<EntryPoint>]
let main args =

    let app = CommandApp<GenerateCommand>()

    app
        .WithDescription(
            "Generate changelog based on the Git history.

Learn more at https://github.com/easybuild-org/EasyBuild.ChangelogGen"
        )
        .Configure(fun config ->
            config.Settings.ApplicationName <- "changelog-gen" // Find a name
        )

    app.Run(args)
