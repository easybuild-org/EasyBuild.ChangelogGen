module EasyBuild.ChangelogGen.Generate.ReleaseContext

open Semver
open EasyBuild.CommitParser
open EasyBuild.CommitParser.Types
open EasyBuild.ChangelogGen.Generate.Types

let getCommits (settings: GenerateSettings) (changelog: ChangelogInfo) =
    let commitFilter =
        match settings.From, settings.To with
        | Some from, Some to_ -> Git.GetCommitsFilter.Between(from, to_)
        | Some from, None -> Git.GetCommitsFilter.From(from)
        | None, Some to_ -> Git.GetCommitsFilter.To to_
        | None, None ->
            match changelog.LastReleaseCommit with
            | Some lastReleasedCommit -> Git.GetCommitsFilter.From lastReleasedCommit
            | None -> Git.GetCommitsFilter.All

    Git.getCommits commitFilter

let compute
    (settings: GenerateSettings)
    (changelog: ChangelogInfo)
    (commitsCandidates: Git.Commit list)
    (commitParserConfig: CommitParserConfig)
    =

    let commitsForRelease =
        commitsCandidates
        // Parse the commit message
        |> List.choose (fun commit ->
            match Parser.tryParseCommitMessage commitParserConfig commit.RawBody with
            | Ok semanticCommit ->
                Some
                    {
                        OriginalCommit = commit
                        SemanticCommit = semanticCommit
                    }
            | Error error ->
                if settings.SkipInvalidCommit then
                    Log.warning $"Failed to parse commit message: {error}"
                    None
                else
                    failwith error
        )
        // Only include commits that have the types we are looking for
        |> List.filter (fun commits ->
            settings.CommitTypes |> Array.contains commits.SemanticCommit.Type
        )
        // Only keep the commits that have the tags we are looking for
        // or all commits if no tags are provided
        |> List.filter (fun commit ->
            // If no tags are provided, include all commits
            if settings.Tags.Length = 0 then
                true
            else
                settings.Tags
                |> Array.exists (fun searchedTag ->
                    match commit.SemanticCommit.Tags with
                    | None -> false
                    | Some commitTags -> commitTags |> List.contains searchedTag
                )
        )

    let shouldBumpMajor =
        commitsForRelease
        |> List.exists (fun commit -> commit.SemanticCommit.BreakingChange)

    let shouldBumpMinor =
        commitsForRelease
        |> List.exists (fun commit -> commit.SemanticCommit.Type = "feat")

    let shouldBumpPatch =
        commitsForRelease
        |> List.exists (fun commit -> commit.SemanticCommit.Type = "fix")

    let refVersion = changelog.LastVersion

    let makeVersionBump newVersion =
        {
            NewVersion = newVersion
            CommitsForRelease = commitsForRelease
            LastCommitSha = commitsCandidates[0].Hash
        }

    let applyPreRelease (newVersion : SemVersion) =
        if settings.PreRelease.IsSet then
            newVersion.WithPrerelease(settings.PreRelease.Value)
        else
            newVersion.WithPrerelease("")

    let bumpMajor () =
        refVersion.WithMajor(refVersion.Major + 1).WithMinor(0).WithPatch(0)
        |> applyPreRelease
        |> makeVersionBump
        |> BumpRequired
        |> Ok

    let bumpMinor () =
        refVersion.WithMinor(refVersion.Minor + 1).WithPatch(0)
        |> applyPreRelease
        |> makeVersionBump
        |> BumpRequired
        |> Ok

    let bumpPatch () =
        refVersion.WithPatch(refVersion.Patch + 1)

        |> makeVersionBump
        |> BumpRequired
        |> Ok

    match settings.ForceVersion with
    | Some version ->
        SemVersion.Parse(version, SemVersionStyles.Strict)
        |> makeVersionBump
        |> BumpRequired
        |> Ok
    | None ->
        match settings.BumpMajor, settings.BumpMinor, settings.BumpPatch with
        | false, false, false ->
            if shouldBumpMajor then
                bumpMajor()
            elif shouldBumpMinor then
                bumpMinor()
            elif shouldBumpPatch then
                bumpPatch()
            else
                Ok NoVersionBumpRequired

        | true, false, false ->
            bumpMajor()
        | false, true, false ->
            bumpMinor()
        | false, false, true ->
            bumpPatch()
        | _ -> Error "Only one of --major, --minor, or --patch can be used at a time."
