module EasyBuild.ChangelogGen.Generate.ReleaseContext

open Semver
open EasyBuild.CommitParser
open EasyBuild.CommitParser.Types
open EasyBuild.ChangelogGen.Generate.Types

let getCommits (settings: GenerateSettings) (changelog: ChangelogInfo) =
    let commitFilter =
        match changelog.LastReleaseCommit with
        | Some lastReleasedCommit -> Git.GetCommitsFilter.From lastReleasedCommit
        | None -> Git.GetCommitsFilter.All

    Git.getCommits commitFilter

let computeVersion
    (settings: GenerateSettings)
    (commitsForRelease: CommitForRelease list)
    (refVersion: SemVersion)
    =

    if refVersion.IsPrerelease then
        if settings.PreRelease.IsSet then

            // If the pre-release identifier is the same, then increment the pre-release number
            // Before: 2.0.0-beta.1 -> After: 2.0.0-beta.2
            if refVersion.Prerelease.StartsWith(settings.PreRelease.Value) then
                let index = refVersion.Prerelease.IndexOf(settings.PreRelease.Value + ".")

                if index >= 0 then
                    let preReleaseNumber =
                        refVersion.Prerelease.Substring(
                            index + settings.PreRelease.Value.Length + 1
                        )
                        |> int

                    refVersion.WithPrereleaseParsedFrom(
                        settings.PreRelease.Value + "." + (preReleaseNumber + 1).ToString()
                    )
                    |> Some
                else
                    // This should never happen
                    // If the pre-release identifier is present, then the pre-release number should also be present
                    // If the pre-release identifier is not present, then the version should be a release
                    // So, this should be a safe assumption
                    failwith "Invalid pre-release identifier"
            // Otherwise, start a new pre-release from 1
            // This can happens when moving from alpha to beta, for example
            // Before: 2.0.0-alpha.1 -> After: 2.0.0-beta.1
            else
                refVersion.WithPrereleaseParsedFrom(settings.PreRelease.Value + ".1") |> Some
        // If the last version is a release, and user requested a stable release
        // Then, remove the pre-release identifier
        // Example: 2.0.0-beta.1 -> 2.0.0
        else
            refVersion.WithoutPrereleaseOrMetadata() |> Some

    else
        let shouldBumpMajor =
            commitsForRelease
            |> List.exists (fun commit -> commit.SemanticCommit.BreakingChange)

        let shouldBumpMinor =
            commitsForRelease
            |> List.exists (fun commit ->
                commit.SemanticCommit.Type = "feat" || commit.SemanticCommit.Type = "perf"
            )

        let shouldBumpPatch =
            commitsForRelease
            |> List.exists (fun commit -> commit.SemanticCommit.Type = "fix")

        let bumpMajor () =
            refVersion
                .WithMajor(refVersion.Major + 1)
                .WithMinor(0)
                .WithPatch(0)
                .WithoutPrereleaseOrMetadata()

        let bumpMinor () =
            refVersion
                .WithMinor(refVersion.Minor + 1)
                .WithPatch(0)
                .WithoutPrereleaseOrMetadata()

        let bumpPatch () =
            refVersion.WithPatch(refVersion.Patch + 1).WithoutPrereleaseOrMetadata()

        // If the last version is a release, and user requested a pre-release
        // Then we compute the standard release version and add the pre-release identifier starting from 1
        // Example:
        // - Major bump needed: 2.0.0 -> 3.0.0-beta.1
        // - Minor bump needed: 2.0.0 -> 2.1.0-beta.1
        // - Patch bump needed: 2.0.0 -> 2.0.1-beta.1
        if settings.PreRelease.IsSet then
            let applyPreReleaseIdentifier (version: SemVersion) =
                version.WithPrereleaseParsedFrom(settings.PreRelease.Value + ".1")

            if shouldBumpMajor then
                bumpMajor () |> applyPreReleaseIdentifier |> Some
            elif shouldBumpMinor then
                bumpMinor () |> applyPreReleaseIdentifier |> Some
            elif shouldBumpPatch then
                bumpPatch () |> applyPreReleaseIdentifier |> Some
            else
                None

        // If the last version is a release, and user requested a stable release
        // Then we compute the standard release version
        // Example:
        // - Major bump needed: 2.0.0 -> 3.0.0
        // - Minor bump needed: 2.0.0 -> 2.1.0
        // - Patch bump needed: 2.0.0 -> 2.0.1
        else
            let removePreReleaseIdentifier (version: SemVersion) =
                version.WithoutPrereleaseOrMetadata()

            if shouldBumpMajor then
                bumpMajor () |> removePreReleaseIdentifier |> Some
            elif shouldBumpMinor then
                bumpMinor () |> removePreReleaseIdentifier |> Some
            elif shouldBumpPatch then
                bumpPatch () |> removePreReleaseIdentifier |> Some
            else
                None

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
                else if settings.SkipMergeCommit && commit.RawBody.StartsWith("Merge ") then
                    // Skip merge commits
                    None
                else
                    failwith error
        )
        // Only include commits that have the type feat, fix or is marked as a breaking change
        |> List.filter (fun commit ->
            commit.SemanticCommit.Type = "feat"
            || commit.SemanticCommit.Type = "perf"
            || commit.SemanticCommit.Type = "fix"
            || commit.SemanticCommit.BreakingChange
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
                    match Map.tryFind "Tag" commit.SemanticCommit.Footers with
                    | Some tags -> tags |> List.contains searchedTag
                    | None -> false
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
        match computeVersion settings commitsForRelease refVersion with
        | Some newVersion -> makeBumpInfo newVersion |> BumpRequired
        | None -> NoVersionBumpRequired
