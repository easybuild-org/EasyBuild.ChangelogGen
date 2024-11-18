module EasyBuild.ChangelogGen.Generate.Types

open Spectre.Console.Cli
open System.ComponentModel
open System.IO
open Semver
open EasyBuild.CommitParser.Types
open System.Text.RegularExpressions

type GenerateSettings() =
    inherit CommandSettings()

    [<CommandArgument(0, "[changelog]")>]
    [<Description("Path to the changelog file. Default is CHANGELOG.md")>]
    member val Changelog: string = "CHANGELOG.md" with get, set

    [<CommandOption("-c|--config")>]
    [<Description("Path to the configuration file")>]
    member val Config: string option = None with get, set

    [<CommandOption("--allow-dirty")>]
    [<Description("Allow to run in a dirty repository (having not commit changes in your reporitory)")>]
    member val AllowDirty: bool = false with get, set

    [<CommandOption("--allow-branch <VALUES>")>]
    [<Description("List of branches that are allowed to be used to generate the changelog. Default is 'main'")>]
    member val AllowBranch: string array = [| "main" |] with get, set

    [<CommandOption("--tag <VALUES>")>]
    [<Description("List of tags to include in the changelog")>]
    member val Tags: string array = [||] with get, set

    [<CommandOption("--pre-release [prefix]")>]
    [<DefaultValue("beta")>]
    [<Description("Indicate that the generated version is a pre-release version. Optionally, you can provide a prefix for the beta version. Default is 'beta'")>]
    member val PreRelease: FlagValue<string> = FlagValue() with get, set

    [<CommandOption("--force-version <VERSION>")>]
    [<Description("Force the version to be used in the changelog")>]
    member val ForceVersion: string option = None with get, set

    [<CommandOption("--skip-invalid-commit")>]
    [<Description("Skip invalid commits instead of failing")>]
    member val SkipInvalidCommit: bool = true with get, set

    [<CommandOption("--dry-run")>]
    [<Description("Run the command without writing to the changelog file, output the result in STDOUT instead")>]
    member val DryRun: bool = false with get, set

    [<CommandOption("--github-repo <REPO>")>]
    [<Description("GitHub repository name in format 'owner/repo'")>]
    member val GitHubRepo: string option = None with get, set

type CommitForRelease =
    {
        OriginalCommit: Git.Commit
        SemanticCommit: CommitMessage
    }

type BumpInfo =
    {
        NewVersion: SemVersion
        CommitsForRelease: CommitForRelease list
        LastCommitSha: string
    }

type ReleaseContext =
    | NoVersionBumpRequired
    | BumpRequired of BumpInfo

type ChangelogInfo =
    {
        File: FileInfo
        Content: string
        Versions: SemVersion list
    }

    member this.LastVersion =
        match List.tryHead this.Versions with
        | Some version -> version
        | None -> SemVersion(0, 0, 0)

    member this.Lines = this.Content.Replace("\r\n", "\n").Split('\n')

    member this.LastReleaseCommit =
        let changelogConfigSection =
            this.Lines
            |> Array.skipWhile (fun line -> "<!-- EasyBuild: START -->" <> line)
            |> Array.takeWhile (fun line -> "<!-- EasyBuild: END -->" <> line)

        let regex = Regex("^<!-- last_commit_released:\s(?'hash'\w*) -->$")

        changelogConfigSection
        |> Array.tryPick (fun line ->
            let m = regex.Match(line)

            if m.Success then
                Some m.Groups.["hash"].Value
            else
                None
        )
