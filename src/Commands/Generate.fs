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
                let! config = ConfigLoader.tryLoadConfig settings.Config
                // Apply automatic resolution of remote config if needed
                let! remoteConfig = Verify.resolveRemoteConfig settings

                let! changelogInfo = Changelog.load settings
                do! Verify.dirty settings
                do! Verify.branch settings
                // do! Verify.options settings changelogInfo

                let commits = ReleaseContext.getCommits settings changelogInfo

                let releaseContext =
                    ReleaseContext.compute settings changelogInfo commits config.CommitParserConfig

                match releaseContext with
                | NoVersionBumpRequired ->
                    Log.success "No version bump required."
                    return 101
                | BumpRequired bumpInfo ->
                    if settings.DryRun then
                        let newVersionContent =
                            Changelog.generateNewVersionSection
                                remoteConfig
                                changelogInfo.LastReleaseCommit
                                bumpInfo

                        Log.info "Dry run enabled, new version content:"
                        printfn "%s" newVersionContent
                        return 0
                    else
                        let newChangelogContent =
                            Changelog.updateWithNewVersion remoteConfig bumpInfo changelogInfo

                        File.WriteAllText(changelogInfo.File.FullName, newChangelogContent)
                        Log.success ($"Changelog updated with new version:")
                        // Print to stdout so it can be captured easily by other tools
                        printfn "%s" (bumpInfo.NewVersion.ToString())
                        return 0
            }

        match res with
        | Ok exitCode -> exitCode
        | Error error ->
            Log.error error
            1
