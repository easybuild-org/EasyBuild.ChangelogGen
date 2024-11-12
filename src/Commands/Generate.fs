module EasyBuild.ChangelogGen.Commands.Generate

open Spectre.Console.Cli
open FsToolkit.ErrorHandling
open EasyBuild.ChangelogGen
open EasyBuild.ChangelogGen.Generate
open EasyBuild.ChangelogGen.Generate.Types

type GenerateCommand() =
    inherit Command<GenerateSettings>()

    interface ICommandLimiter<GenerateSettings>

    override __.Execute(context, settings) =

        let res =
            result {
                let! config = ConfigLoader.tryLoadConfig settings.Cwd settings.Config
                let! changelogInfo = Changelog.load settings
                do! Verify.dirty settings
                do! Verify.branch settings
                let commits = ReleaseContext.getCommits settings changelogInfo
                let! releaseContext = ReleaseContext.compute settings changelogInfo commits config.CommitParserConfig

                // match releaseContext with
                // | NoVersionBumpRequired -> Log.success "No version bump required."
                // | BumpRequired bumpInfo ->
                //     Log.info $"Bumping version to {bumpInfo.NewVersion}"
                ()
            }

        match res with
        | Ok _ -> 0
        | Error error ->
            Log.error error
            1
