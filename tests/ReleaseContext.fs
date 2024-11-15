module EasyBuild.ChangelogGen.Tests.ReleaseContext

open Expecto
open Tests.Utils
open EasyBuild.ChangelogGen.Generate
open EasyBuild.ChangelogGen.Generate.Types
open System.IO
open Semver
open EasyBuild.CommitParser
open EasyBuild.CommitParser.Types
open FsToolkit.ErrorHandling
open Spectre.Console.Cli

[<Literal>]
let STANDARD_CHANGELOG =
    """# Changelog

All notable changes to this project will be documented in this file.

This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

This changelog is generated using [EasyBuild.ChangelogGen](https://github.com/easybuild-org/EasyBuild.ChangelogGen). Do not edit this file manually.

<!-- EasyBuild: START -->
<!-- EasyBuild: END -->

## 0.0.0

<!--
This version is here for programs like [EasyBuild.PackageReleaseNotes.Tasks](https://github.com/easybuild-org/EasyBuild.PackageReleaseNotes.Tasks)
to be able to build your project when working on the first version.
-->
"""

let private computeTests =
    testList "compute" [
        test "No version bump required if no commits" {
            let defaultGenerateSettings = GenerateSettings(Changelog = "CHANGELOG.md")

            let changelogInfo =
                {
                    File = FileInfo(Path.GetTempFileName())
                    Content = STANDARD_CHANGELOG
                    Versions = [ SemVersion(0, 0, 0) ]
                }

            let actual =
                ReleaseContext.compute
                    defaultGenerateSettings
                    changelogInfo
                    []
                    CommitParserConfig.Default

            Expect.equal actual NoVersionBumpRequired
        }

        test "No version bump required if no commits have a suitable type" {

            let defaultGenerateSettings = GenerateSettings(Changelog = "CHANGELOG.md")

            let changelogInfo =
                {
                    File = FileInfo(Path.GetTempFileName())
                    Content = STANDARD_CHANGELOG
                    Versions = [ SemVersion(0, 0, 0) ]
                }

            let commits: Git.Commit list =
                [
                    Git.Commit.Create(
                        "49c0699af98a67f1e8efcac8b1467b283a244aa8",
                        "chore: update dependencies"
                    )
                    Git.Commit.Create(
                        "43c60e4fc9585a9f235ab6a6dd97c4c1cf945e46",
                        "test: add a lot of tests"
                    )
                    Git.Commit.Create(
                        "f3c60e4fc9585a9f235ab6a6dd97c4c1cf945e46",
                        "style: fix formatting"
                    )
                    Git.Commit.Create(
                        "24da54e481726924b8cb03c28fe0821141883c28",
                        "refactor: refactor code"
                    )
                    Git.Commit.Create(
                        "e468776fc99ec895bf6942c8e2f16d02bbbd6e61",
                        "docs: update documentation"
                    )
                    Git.Commit.Create(
                        "95a0f02adc4a69e2d3f516cb11c1be8a4ef5c803",
                        "ci: update CI/CD configuration"
                    )
                ]

            let actual =
                ReleaseContext.compute
                    defaultGenerateSettings
                    changelogInfo
                    commits
                    CommitParserConfig.Default

            Expect.equal actual NoVersionBumpRequired
        }

        test "If commit is of type feat bump minor" {
            let defaultGenerateSettings = GenerateSettings(Changelog = "CHANGELOG.md")

            let changelogInfo =
                {
                    File = FileInfo(Path.GetTempFileName())
                    Content = STANDARD_CHANGELOG
                    Versions = [ SemVersion(0, 0, 0) ]
                }

            let commits: Git.Commit list =
                [
                    Git.Commit.Create(
                        "49c0699af98a67f1e8efcac8b1467b283a244aa8",
                        "feat: add a new feature"
                    )
                ]

            let actual =
                ReleaseContext.compute
                    defaultGenerateSettings
                    changelogInfo
                    commits
                    CommitParserConfig.Default

            let expected =
                {
                    NewVersion = SemVersion(0, 1, 0)
                    CommitsForRelease =
                        [
                            {
                                OriginalCommit = commits[0]
                                SemanticCommit =
                                    Parser.tryParseCommitMessage
                                        CommitParserConfig.Default
                                        commits[0].ShortMessage
                                    |> Result.valueOr failwith
                            }
                        ]
                    LastCommitSha = "49c0699af98a67f1e8efcac8b1467b283a244aa8"
                }
                |> BumpRequired

            Expect.equal actual expected
        }

        test "If commit is of type fix bump patch" {
            let defaultGenerateSettings = GenerateSettings(Changelog = "CHANGELOG.md")

            let changelogInfo =
                {
                    File = FileInfo(Path.GetTempFileName())
                    Content = STANDARD_CHANGELOG
                    Versions = [ SemVersion(0, 0, 0) ]
                }

            let commits: Git.Commit list =
                [
                    Git.Commit.Create(
                        "49c0699af98a67f1e8efcac8b1467b283a244aa8",
                        "fix: fix a bug"
                    )
                ]

            let actual =
                ReleaseContext.compute
                    defaultGenerateSettings
                    changelogInfo
                    commits
                    CommitParserConfig.Default

            let expected =
                {
                    NewVersion = SemVersion(0, 0, 1)
                    CommitsForRelease =
                        [
                            {
                                OriginalCommit = commits[0]
                                SemanticCommit =
                                    Parser.tryParseCommitMessage
                                        CommitParserConfig.Default
                                        commits[0].ShortMessage
                                    |> Result.valueOr failwith
                            }
                        ]
                    LastCommitSha = "49c0699af98a67f1e8efcac8b1467b283a244aa8"
                }
                |> BumpRequired

            Expect.equal actual expected
        }

        test "If --force-version is set, use that version" {
            let defaultGenerateSettings =
                GenerateSettings(Changelog = "CHANGELOG.md", ForceVersion = Some "4.9.3")

            let changelogInfo =
                {
                    File = FileInfo(Path.GetTempFileName())
                    Content = STANDARD_CHANGELOG
                    Versions = [ SemVersion(0, 0, 0) ]
                }

            let commits: Git.Commit list =
                [
                    Git.Commit.Create(
                        "49c0699af98a67f1e8efcac8b1467b283a244aa8",
                        "feat: add a new feature"
                    )
                    Git.Commit.Create(
                        "43c60e4fc9585a9f235ab6a6dd97c4c1cf945e46",
                        "fix: fix a bug"
                    )
                ]

            let actual =
                ReleaseContext.compute
                    defaultGenerateSettings
                    changelogInfo
                    commits
                    CommitParserConfig.Default

            match actual with
            | BumpRequired { NewVersion = version } ->
                Expect.equal version (SemVersion(4, 9, 3))
            | _ -> failtest "Expected BumpRequired"
        }

        test "If --minor is set, bump minor" {
            let defaultGenerateSettings =
                GenerateSettings(Changelog = "CHANGELOG.md", BumpMinor = true)

            let changelogInfo =
                {
                    File = FileInfo(Path.GetTempFileName())
                    Content = STANDARD_CHANGELOG
                    Versions = [ SemVersion(2, 3, 0) ]
                }

            let commits: Git.Commit list =
                [
                    Git.Commit.Create(
                        "43c60e4fc9585a9f235ab6a6dd97c4c1cf945e46",
                        "fix: fix a bug"
                    )
                ]

            let actual =
                ReleaseContext.compute
                    defaultGenerateSettings
                    changelogInfo
                    commits
                    CommitParserConfig.Default

            match actual with
            | BumpRequired { NewVersion = version } ->
                Expect.equal version (SemVersion(2, 4, 0))
            | _ -> failtest "Expected BumpRequired"
        }

        test "If --patch is set, bump patch" {
            let defaultGenerateSettings =
                GenerateSettings(Changelog = "CHANGELOG.md", BumpPatch = true)

            let changelogInfo =
                {
                    File = FileInfo(Path.GetTempFileName())
                    Content = STANDARD_CHANGELOG
                    Versions = [ SemVersion(2, 3, 0) ]
                }

            let commits: Git.Commit list =
                [
                    Git.Commit.Create(
                        "43c60e4fc9585a9f235ab6a6dd97c4c1cf945e46",
                        "feat: add a new feature"
                    )
                ]

            let actual =
                ReleaseContext.compute
                    defaultGenerateSettings
                    changelogInfo
                    commits
                    CommitParserConfig.Default

            match actual with
            | BumpRequired { NewVersion = version } ->
                Expect.equal version (SemVersion(2, 3, 1))
            | _ -> failtest "Expected BumpRequired"
        }

        test "If --major is set, bump major" {
            let defaultGenerateSettings =
                GenerateSettings(Changelog = "CHANGELOG.md", BumpMajor = true)

            let changelogInfo =
                {
                    File = FileInfo(Path.GetTempFileName())
                    Content = STANDARD_CHANGELOG
                    Versions = [ SemVersion(2, 3, 0) ]
                }

            let commits: Git.Commit list =
                [
                    Git.Commit.Create(
                        "43c60e4fc9585a9f235ab6a6dd97c4c1cf945e46",
                        "feat: add a new feature"
                    )
                ]

            let actual =
                ReleaseContext.compute
                    defaultGenerateSettings
                    changelogInfo
                    commits
                    CommitParserConfig.Default

            match actual with
            | BumpRequired { NewVersion = version } ->
                Expect.equal version (SemVersion(3, 0, 0))
            | _ -> failtest "Expected BumpRequired"
        }
    ]

let private computePreReleaseVersionTests =
    testList "computePreReleaseVersion" [
        test "If previous version was stable, start a new pre-release from 1" {
            let settings =
                GenerateSettings(PreRelease = FlagValue(Value = "beta", IsSet = true))

            let actual =
                ReleaseContext.computePreReleaseVersion
                    settings
                    (SemVersion(1, 2, 3))

            Expect.equal actual (SemVersion(2,0,0).WithPrereleaseParsedFrom("beta.1"))
        }

        test "If previous version was a pre-release with same identifier, increment the number" {
            let settings =
                GenerateSettings(PreRelease = FlagValue(Value = "beta", IsSet = true))

            let actual =
                ReleaseContext.computePreReleaseVersion
                    settings
                    (SemVersion(2,0,0).WithPrereleaseParsedFrom("beta.1"))

            Expect.equal actual (SemVersion(2,0,0).WithPrereleaseParsedFrom("beta.2"))

            // Check if works for multi-digit numbers
            let actual =
                ReleaseContext.computePreReleaseVersion
                    settings
                    (SemVersion(2,0,0).WithPrereleaseParsedFrom("beta.7832"))

            Expect.equal actual (SemVersion(2,0,0).WithPrereleaseParsedFrom("beta.7833"))
        }

        test "If previous version was a pre-release with different identifier, start a new pre-release from 1" {
            let settings =
                GenerateSettings(PreRelease = FlagValue(Value = "beta", IsSet = true))

            let actual =
                ReleaseContext.computePreReleaseVersion
                    settings
                    (SemVersion(2,0,0).WithPrereleaseParsedFrom("alpha.1"))

            Expect.equal actual (SemVersion(2,0,0).WithPrereleaseParsedFrom("beta.1"))
        }
    ]

let private computeReleaseVersionTests =
    testList "computeReleaseVersionTests" [
        test "if previous version was a pre-release, release it as stable"{
            let settings = GenerateSettings(Changelog = "CHANGELOG.md")

            let commits: Git.Commit list =
                [
                    Git.Commit.Create(
                        "49c0699af98a67f1e8efcac8b1467b283a244aa8",
                        "fix: fix a bug"
                    )
                ]

            let commits: CommitForRelease list =
                [
                    {
                        OriginalCommit = commits[0]
                        SemanticCommit =
                            Parser.tryParseCommitMessage
                                CommitParserConfig.Default
                                commits[0].ShortMessage
                            |> Result.valueOr failwith
                    }
                ]

            let actual =
                ReleaseContext.computeReleaseVersion
                    settings
                    commits
                    (SemVersion(2, 0, 0).WithPrereleaseParsedFrom("beta.1"))

            Expect.equal actual (Some (SemVersion(2, 0, 0)))
        }
    ]

let tests =
    testList
        "ReleaseContext"
        [
            computeTests
            computePreReleaseVersionTests
            computeReleaseVersionTests
        ]
