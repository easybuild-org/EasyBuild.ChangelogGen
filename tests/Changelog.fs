module EasyBuild.ChangelogGen.Tests.Changelog

open Workspace
open Expecto
open Tests.Utils
open EasyBuild.ChangelogGen.Types
open EasyBuild.ChangelogGen.Generate
open EasyBuild.ChangelogGen.Generate.Types
open EasyBuild.CommitParser
open EasyBuild.CommitParser.Types

open type TestHelper

let private loadTests =
    testList
        "Changelog.load"
        [
            test "works if no changelog file exists" {
                let actual =
                    let settings = GenerateSettings(Changelog = "THIS_CHANGELOG_DOENST_EXIST.md")

                    Changelog.load settings

                match actual with
                | Ok actual ->
                    Expect.equal actual.Content Changelog.EMPTY_CHANGELOG
                    Expect.equal actual.LastVersion (Semver.SemVersion(0, 0, 0))
                | Error _ -> failwith "Expected Ok"
            }

            test "works if changelog file exists" {
                let actual =
                    let settings = GenerateSettings(Changelog = Workspace.``valid_changelog.md``)

                    Changelog.load settings

                match actual with
                | Ok actual ->
                    Expect.isNotEmpty actual.Content
                    Expect.equal actual.LastVersion (Semver.SemVersion(1, 0, 0))
                | Error _ -> failwith "Expected Ok"
            }

            test "works for changelog without version" {
                let actual =
                    let settings = GenerateSettings(Changelog = Workspace.``valid_no_version.md``)

                    Changelog.load settings

                match actual with
                | Ok actual ->
                    Expect.isNotEmpty actual.Content
                    Expect.equal actual.LastVersion (Semver.SemVersion(0, 0, 0))
                | Error _ -> failwith "Expected Ok"
            }
        ]

let private tryFindAdditionalChangelogContentTests =
    testList
        "Changelog.tryFindAdditionalChangelogContent"
        [
            test "works if no additional content is found" {
                let actual = Changelog.tryFindAdditionalChangelogContent "Some content"

                Expect.equal actual []
            }

            test "works if additional content is found" {
                let actual =
                    Changelog.tryFindAdditionalChangelogContent
                        """Some content

=== changelog ===
This goes into the changelog
=== changelog ===
                """

                Expect.equal actual [ [ "This goes into the changelog" ] ]
            }

            test "works for multiple additional content blocks" {
                let actual =
                    Changelog.tryFindAdditionalChangelogContent
                        """Some content

=== changelog ===
This goes into the changelog
=== changelog ===

Some more content

=== changelog ===
This goes into the changelog as well
=== changelog ===
                """

                Expect.equal
                    actual
                    [
                        [ "This goes into the changelog" ]
                        [ "This goes into the changelog as well" ]
                    ]
            }
        ]

let gitCommitToCommitForRelease (commit: Git.Commit) =
    {
        OriginalCommit = commit
        SemanticCommit =
            Parser.tryParseCommitMessage CommitParserConfig.Default commit.RawBody
            |> function
                | Ok semanticCommit -> semanticCommit
                | Error error -> failwith error
    }

let private generateNewVersionSectionTests =
    testList
        "Changelog.generateNewVersionSection"
        [
            testMarkdown (
                "works for feat type commit",
                (fun _ ->
                    Changelog.generateNewVersionSection
                        ChangelogGenConfig.Default
                        {
                            NewVersion = Semver.SemVersion(1, 0, 0)
                            CommitsForRelease =
                                [
                                    Git.Commit.Create(
                                        "0b1899bb03d3eb86a30c84aa4c66c037527fbd14",
                                        "feat: Add feature"
                                    )
                                    |> gitCommitToCommitForRelease
                                    Git.Commit.Create(
                                        "2a6f3b3403aaa629de6e65558448b37f126f8e86",
                                        "feat: Add another feature"
                                    )
                                    |> gitCommitToCommitForRelease
                                ]
                            LastCommitSha = "0b1899bb03d3eb86a30c84aa4c66c037527fbd14"
                        }
                )
            )

            testMarkdown (
                "works for fix type commit",
                (fun _ ->
                    Changelog.generateNewVersionSection
                        ChangelogGenConfig.Default
                        {
                            NewVersion = Semver.SemVersion(1, 0, 0)
                            CommitsForRelease =
                                [
                                    Git.Commit.Create(
                                        "0b1899bb03d3eb86a30c84aa4c66c037527fbd14",
                                        "fix: Fix bug"
                                    )
                                    |> gitCommitToCommitForRelease
                                    Git.Commit.Create(
                                        "2a6f3b3403aaa629de6e65558448b37f126f8e86",
                                        "fix: Fix another bug"
                                    )
                                    |> gitCommitToCommitForRelease
                                ]
                            LastCommitSha = "0b1899bb03d3eb86a30c84aa4c66c037527fbd14"
                        }
                )
            )

            testMarkdown (
                "breaking change are going into their own section if configured",
                (fun _ ->
                    Changelog.generateNewVersionSection
                        ChangelogGenConfig.Default
                        {
                            NewVersion = Semver.SemVersion(1, 0, 0)
                            CommitsForRelease =
                                [
                                    Git.Commit.Create(
                                        "0b1899bb03d3eb86a30c84aa4c66c037527fbd14",
                                        "feat: Add feature"
                                    )
                                    |> gitCommitToCommitForRelease
                                    Git.Commit.Create(
                                        "2a6f3b3403aaa629de6e65558448b37f126f8e86",
                                        "fix: Fix bug"
                                    )
                                    |> gitCommitToCommitForRelease
                                    Git.Commit.Create(
                                        "9156258d463ba78ac21ebb5fcd32147657bfe86f",
                                        "fix!: Fix bug via breaking change"
                                    )
                                    |> gitCommitToCommitForRelease
                                    Git.Commit.Create(
                                        "4057b1a703845efcdf2f3b49240dd79d8ce7150e",
                                        "feat!: Add another feature via breaking change"
                                    )
                                    |> gitCommitToCommitForRelease
                                ]
                            LastCommitSha = "0b1899bb03d3eb86a30c84aa4c66c037527fbd14"
                        }
                )
            )

            testMarkdown (
                "breaking change stays in their original group if they don't have a dedicated group",
                (fun _ ->
                    Changelog.generateNewVersionSection
                        {
                            Github =
                                {
                                    Owner = "owner"
                                    Repository = "repository"
                                }
                            Groups =
                                [
                                    {
                                        Type = "feat"
                                        Bump = Minor
                                        Group = "🚀 Features"
                                    }
                                    {
                                        Type = "fix"
                                        Bump = Patch
                                        Group = "🐞 Bug Fixes"
                                    }
                                ]
                        }
                        {
                            NewVersion = Semver.SemVersion(1, 0, 0)
                            CommitsForRelease =
                                [
                                    Git.Commit.Create(
                                        "0b1899bb03d3eb86a30c84aa4c66c037527fbd14",
                                        "feat: Add feature"
                                    )
                                    |> gitCommitToCommitForRelease
                                    Git.Commit.Create(
                                        "2a6f3b3403aaa629de6e65558448b37f126f8e86",
                                        "fix: Fix bug"
                                    )
                                    |> gitCommitToCommitForRelease
                                    Git.Commit.Create(
                                        "9156258d463ba78ac21ebb5fcd32147657bfe86f",
                                        "fix: Fix bug via breaking change"
                                    )
                                    |> gitCommitToCommitForRelease
                                    Git.Commit.Create(
                                        "4057b1a703845efcdf2f3b49240dd79d8ce7150e",
                                        "feat: Add another feature via breaking change"
                                    )
                                    |> gitCommitToCommitForRelease
                                ]
                            LastCommitSha = "0b1899bb03d3eb86a30c84aa4c66c037527fbd14"
                        }
                )
            )

            testMarkdown (
                "only commit with a configured group are included in the changelog",
                (fun _ ->
                    Changelog.generateNewVersionSection
                        {
                            Github =
                                {
                                    Owner = "owner"
                                    Repository = "repository"
                                }
                            Groups =
                                [
                                    {
                                        Type = "chore"
                                        Bump = Minor
                                        Group = "🧹 Chore"
                                    }
                                    {
                                        Type = "style"
                                        Bump = Patch
                                        Group = "🎨 Style"
                                    }
                                ]
                        }
                        {
                            NewVersion = Semver.SemVersion(1, 0, 0)
                            CommitsForRelease =
                                [
                                    Git.Commit.Create(
                                        "0b1899bb03d3eb86a30c84aa4c66c037527fbd14",
                                        "feat: Add feature"
                                    )
                                    |> gitCommitToCommitForRelease
                                    Git.Commit.Create(
                                        "2a6f3b3403aaa629de6e65558448b37f126f8e86",
                                        "fix: Fix bug"
                                    )
                                    |> gitCommitToCommitForRelease
                                    Git.Commit.Create(
                                        "9156258d463ba78ac21ebb5fcd32147657bfe86f",
                                        "chore: Do some chore"
                                    )
                                    |> gitCommitToCommitForRelease
                                    Git.Commit.Create(
                                        "4057b1a703845efcdf2f3b49240dd79d8ce7150e",
                                        "style: Fix style"
                                    )
                                    |> gitCommitToCommitForRelease
                                ]
                            LastCommitSha = "0b1899bb03d3eb86a30c84aa4c66c037527fbd14"
                        }
                )
            )

            testMarkdown (
                "include changelog additional data when present",
                (fun _ ->
                    Changelog.generateNewVersionSection
                        ChangelogGenConfig.Default
                        {
                            NewVersion = Semver.SemVersion(1, 0, 0)
                            CommitsForRelease =
                                [
                                    Git.Commit.Create(
                                        "0b1899bb03d3eb86a30c84aa4c66c037527fbd14",
                                        """feat: Add feature

=== changelog ===
```fs
let upper (s: string) = s.ToUpper()
```
=== changelog ===

=== changelog ===
This is a list of changes:

* Added upper function
* Added lower function
=== changelog ===
                                    """
                                    )
                                    |> gitCommitToCommitForRelease
                                    Git.Commit.Create(
                                        "2a6f3b3403aaa629de6e65558448b37f126f8e86",
                                        "fix: Fix bug"
                                    )
                                    |> gitCommitToCommitForRelease
                                ]
                            LastCommitSha = "0b1899bb03d3eb86a30c84aa4c66c037527fbd14"
                        }
                )
            )
        ]

let tests =
    testList
        "Changelog"
        [
            loadTests
            tryFindAdditionalChangelogContentTests
            generateNewVersionSectionTests
        ]
