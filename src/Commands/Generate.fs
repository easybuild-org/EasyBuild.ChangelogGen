module EasyBuild.ChangelogGen.Commands.Generate

open Spectre.Console.Cli
open FsToolkit.ErrorHandling
open EasyBuild.ChangelogGen
open EasyBuild.ChangelogGen.Generate
open EasyBuild.ChangelogGen.Generate.Types
open System.IO

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
                do! Verify.options settings changelogInfo

                let commits = ReleaseContext.getCommits settings changelogInfo
                let releaseContext = ReleaseContext.compute settings changelogInfo commits config.CommitParserConfig

                match releaseContext with
                | NoVersionBumpRequired -> Log.success "No version bump required."
                | BumpRequired bumpInfo ->
                    let newChangelogContent = Changelog.updateWithNewVersion config.ChangelogGenConfig bumpInfo changelogInfo

                    if settings.DryRun then
                        Log.info "Dry run enabled, not writing to file."
                        printfn "%s" newChangelogContent
                    else
                        File.WriteAllText(changelogInfo.File.FullName, newChangelogContent)
                        Log.success ($"Changelog updated with new version {bumpInfo.NewVersion}.")
            }

        match res with
        | Ok _ -> 0
        | Error error ->
            Log.error error
            1
