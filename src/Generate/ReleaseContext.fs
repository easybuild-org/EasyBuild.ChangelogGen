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

let computePreReleaseVersion (settings: GenerateSettings) (refVersion: SemVersion) =
    // Try to normalize pre-release identifier
    let preReleaseIdentifier = settings.PreRelease.Value.Trim('-')

    // If previous version is a release, then start a new pre-release from 1 and by incrementing the major version
    // Before: 1.0.0 -> After: 2.0.0-beta.1
    if refVersion.IsRelease then
        refVersion
            .WithMajor(refVersion.Major + 1)
            .WithMinor(0)
            .WithPatch(0)
            .WithPrereleaseParsedFrom(preReleaseIdentifier + ".1")
    // If the last version is a pre-release of the same identifier, then increment the pre-release number
    // Before: 2.0.0-beta.1 -> After: 2.0.0-beta.2
    else if refVersion.Prerelease.StartsWith(preReleaseIdentifier) then
        let index = refVersion.Prerelease.IndexOf(preReleaseIdentifier + ".")

        if index >= 0 then
            let preReleaseNumber =
                refVersion.Prerelease.Substring(index + preReleaseIdentifier.Length + 1) |> int

            refVersion.WithPrereleaseParsedFrom(
                preReleaseIdentifier + "." + (preReleaseNumber + 1).ToString()
            )
        else
            // This should never happen
            // If the pre-release identifier is present, then the pre-release number should also be present
            // If the pre-release identifier is not present, then the version should be a release
            // So, this is a safe assumption
            failwith "Invalid pre-release identifier"

    // Otherwise, start a new pre-release from 1
    // This can happens when moving from alpha to beta, for example
    // Before: 2.0.0-alpha.1 -> After: 2.0.0-beta.1
    else
        refVersion.WithPrereleaseParsedFrom(preReleaseIdentifier + ".1")

let computeReleaseVersion
    (settings: GenerateSettings)
    (commitsForRelease: CommitForRelease list)
    (refVersion: SemVersion)
    =

    let shouldBumpMajor =
        commitsForRelease
        |> List.exists (fun commit -> commit.SemanticCommit.BreakingChange)

    let shouldBumpMinor =
        commitsForRelease
        |> List.exists (fun commit -> commit.SemanticCommit.Type = "feat")

    let shouldBumpPatch =
        commitsForRelease
        |> List.exists (fun commit -> commit.SemanticCommit.Type = "fix")

    let bumpMajor () =
        refVersion
            .WithMajor(refVersion.Major + 1)
            .WithMinor(0)
            .WithPatch(0)
            .WithoutPrereleaseOrMetadata()
        |> Some

    let bumpMinor () =
        refVersion
            .WithMinor(refVersion.Minor + 1)
            .WithPatch(0)
            .WithoutPrereleaseOrMetadata()
        |> Some

    let bumpPatch () =
        refVersion.WithPatch(refVersion.Patch + 1).WithoutPrereleaseOrMetadata() |> Some

    match settings.BumpMajor, settings.BumpMinor, settings.BumpPatch with
    | false, false, false ->
        if refVersion.IsPrerelease then
            refVersion.WithoutPrereleaseOrMetadata() |> Some
        elif shouldBumpMajor then
            bumpMajor ()
        elif shouldBumpMinor then
            bumpMinor ()
        elif shouldBumpPatch then
            bumpPatch ()
        else
            None

    | true, false, false -> bumpMajor ()
    | false, true, false -> bumpMinor ()
    | false, false, true -> bumpPatch ()
    | _ -> failwith "Only one of --major, --minor, or --patch can be used at a time."

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

    let refVersion = changelog.LastVersion

    let makeBumpInfo newVersion =
        {
            NewVersion = newVersion
            CommitsForRelease = commitsForRelease
            LastCommitSha = commitsCandidates[0].Hash
        }

    // If the user forced a version, then use that version
    match settings.ForceVersion with
    | Some version ->
        SemVersion.Parse(version, SemVersionStyles.Strict)
        |> makeBumpInfo
        |> BumpRequired

    | None ->
        if settings.PreRelease.IsSet then
            computePreReleaseVersion settings refVersion |> makeBumpInfo |> BumpRequired
        else
            match computeReleaseVersion settings commitsForRelease refVersion with
            | Some newVersion -> makeBumpInfo newVersion |> BumpRequired
            | None -> NoVersionBumpRequired
