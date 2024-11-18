module EasyBuild.ChangelogGen.Main

open Spectre.Console.Cli
open EasyBuild.ChangelogGen.Commands.Generate
open EasyBuild.ChangelogGen.Commands.Version

let mutable helpWasCalled = false

type CustomHelperProvider(settings: ICommandAppSettings) =
    inherit Help.HelpProvider(settings)

    override _.GetUsage
        (model: Help.ICommandModel, command: Help.ICommandInfo)
        : System.Collections.Generic.IEnumerable<Spectre.Console.Rendering.IRenderable>
        =
        helpWasCalled <- true
        base.GetUsage(model, command)

[<EntryPoint>]
let main args =

    let app = CommandApp<GenerateCommand>()

    app
        .WithDescription(
            "Generate changelog based on the Git history.

Learn more at https://github.com/easybuild-org/EasyBuild.ChangelogGen"
        )
        .Configure(fun config ->
            config.Settings.ApplicationName <- "changelog-gen"
            config.SetHelpProvider(CustomHelperProvider(config.Settings))
            config.AddCommand<VersionCommand>("version") |> ignore
        )

    let exitCode = app.Run(args)

    if helpWasCalled then
        // Make it easy for caller to know when help was called
        100
    else
        exitCode
