module EasyBuild.ChangelogGen.Generate.Types

open Spectre.Console.Cli
open System.ComponentModel
open System.IO
open Semver
open EasyBuild.CommitParser.Types
open System.Text.RegularExpressions
open YamlDotNet.Serialization
open YamlDotNet.Serialization.NamingConventions

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

    [<CommandOption("--pre-release [prefix]")>]
    [<DefaultValue("beta")>]
    [<Description("Indicate that the generated version is a pre-release version. Optionally, you can provide a prefix for the beta version. Default is 'beta'")>]
    member val PreRelease: FlagValue<string> = FlagValue() with get, set

    [<CommandOption("--force-version <VERSION>")>]
    [<Description("Force the version to be used in the changelog")>]
    member val ForceVersion: string option = None with get, set

    [<CommandOption("--skip-invalid-commit")>]
    [<Description("Skip invalid commits instead of failing")>]
    [<DefaultValue(true)>]
    member val SkipInvalidCommit: bool = true with get, set

    [<CommandOption("--skip-merge-commit")>]
    [<Description("Skip merge commits when generating the changelog (commit messages starting with 'Merge ')")>]
    [<DefaultValue(true)>]
    member val SkipMergeCommit: bool = true with get, set

    [<CommandOption("--dry-run")>]
    [<Description("Run the command without writing to the changelog file, output the new version content to STDOUT instead")>]
    member val DryRun: bool = false with get, set

    [<CommandOption("--remote-hostname <HOSTNAME>")>]
    [<Description("Git remote hostname, e.g. github.com, gitlab.com")>]
    member val RemoteHostname: string option = None with get, set

    [<CommandOption("--remote-owner <OWNER>")>]
    [<Description("Git remote owner or organization name")>]
    member val RemoteOwner: string option = None with get, set

    [<CommandOption("--remote-repo <REPO>")>]
    [<Description("Git remote repository name")>]
    member val RemoteRepo: string option = None with get, set

    member val GitRepositoryRoot: string = Git.getTopLevelDirectory () with get, set

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

type ChangelogMetadata() =

    [<YamlMember(Alias = "last_commit_released", ApplyNamingConventions = false)>]
    member val LastCommitReleased: string option = None with get, set

    member val Include: string list = [] with get, set

    member val Exclude: string list = [] with get, set

    member this.ToConfiguration() =
        let yamlSerializer =
            SerializerBuilder()
                .ConfigureDefaultValuesHandling(
                    DefaultValuesHandling.OmitEmptyCollections
                    ||| DefaultValuesHandling.OmitDefaults
                )
                .WithNamingConvention(CamelCaseNamingConvention.Instance)
                .Build()

        let removeLastNewLine (text: string) =
            let text = text |> String.normalizeNewLines

            let index = text.LastIndexOf('\n')

            if index = text.Length - 1 then
                text.Substring(0, index)
            else
                text

        yamlSerializer.Serialize(this) |> removeLastNewLine

    static member Load(content: string) =
        // First, we try to detect the old metadata format and parse it accordingly
        // Otherwise, we fall back to the new format

        let oldMetadataContent =
            content
            |> String.normalizeNewLines
            |> String.splitBy '\n'
            |> List.skipWhile (fun line -> "<!-- EasyBuild: START -->" <> line)
            |> List.takeWhile (fun line -> "<!-- EasyBuild: END -->" <> line)

        let hasOldMetadataFormat = not oldMetadataContent.IsEmpty

        if hasOldMetadataFormat then
            let lastCommitReleasedRegex =
                Regex("^<!-- last_commit_released:\s(?'hash'\w*) -->$")

            let lastCommitReleased =
                oldMetadataContent
                |> List.tryPick (fun line ->
                    let m = lastCommitReleasedRegex.Match(line)

                    if m.Success then
                        Some m.Groups.["hash"].Value
                    else
                        None
                )

            let metadata = ChangelogMetadata()
            metadata.LastCommitReleased <- lastCommitReleased
            metadata

        else

            let metadataRegex =
                Regex(
                    "^<!-- EasyBuild: START(?'metadata'.*)EasyBuild: END -->$",
                    RegexOptions.Singleline ||| RegexOptions.Multiline
                )

            let m = metadataRegex.Match(content)

            if m.Success then
                let metadataText = m.Groups.["metadata"].Value.Trim()

                let yamlDeserializer =
                    DeserializerBuilder()
                        .WithNamingConvention(CamelCaseNamingConvention.Instance)
                        .Build()

                yamlDeserializer.Deserialize<ChangelogMetadata>(metadataText)
            else
                ChangelogMetadata()

module private Line =

    let isNotVersion (line: string) = not (line.StartsWith("##"))

    let isStartMetadataMarker (line: string) =
        line.StartsWith("<!-- EasyBuild: START")

type ChangelogInfo =
    {
        File: FileInfo
        Content: string
        Versions: SemVersion list
        Metadata: ChangelogMetadata
    }

    member this.LastVersion =
        match List.tryHead this.Versions with
        | Some version -> version
        | None -> SemVersion(0, 0, 0)

    member this.Lines = this.Content |> String.normalizeNewLines |> String.splitBy '\n'

    member this.Description =
        let hasEasyBuildMetadata = this.Lines |> Seq.exists Line.isStartMetadataMarker

        let lines =
            if hasEasyBuildMetadata then
                this.Lines |> Seq.takeWhile (Line.isStartMetadataMarker >> not)
            else
                this.Lines |> Seq.takeWhile Line.isNotVersion

        lines |> String.concat "\n"

    member this.VersionsText =
        this.Lines |> Seq.skipWhile Line.isNotVersion |> String.concat "\n"
