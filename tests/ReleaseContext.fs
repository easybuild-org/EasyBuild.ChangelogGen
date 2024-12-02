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
    testList
        "compute"
        [
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
                                            commits[0].RawBody
                                        |> Result.valueOr failwith
                                }
                            ]
                        LastCommitSha = "49c0699af98a67f1e8efcac8b1467b283a244aa8"
                    }
                    |> BumpRequired

                Expect.equal actual expected
            }

            test "If commit is of type perf bump minor" {
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
                            "perf: i am speed !!!"
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
                                            commits[0].RawBody
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
                                            commits[0].RawBody
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

            test "If tag filter is set, only include commits with one of the requested tag" {
                let defaultGenerateSettings =
                    GenerateSettings(Changelog = "CHANGELOG.md", Tags = [| "converter" |])

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
                            "fix: fix a bug",
                            "fix: fix a bug

Tag: converter"
                        )
                        Git.Commit.Create(
                            "43c60e4fc9585a9f235ab6a6dd97c4c1cf945e46",
                            "feat: add a new feature",
                            "feat: add a new feature

Tag: cli"
                        )
                        Git.Commit.Create(
                            "b7eafe7744e4738d9578c09e1d128bbb2f5c40d3",
                            "feat: add another feature",
                            "feat: add another feature

Tag: cli"
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
                                            commits[0].RawBody
                                        |> Result.valueOr failwith
                                }
                            ]
                        LastCommitSha = "49c0699af98a67f1e8efcac8b1467b283a244aa8"
                    }
                    |> BumpRequired

                Expect.equal actual expected
            }

            test "several tags can be provided" {
                let defaultGenerateSettings =
                    GenerateSettings(
                        Changelog = "CHANGELOG.md",
                        Tags =
                            [|
                                "converter"
                                "cli"
                            |]
                    )

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
                            "fix: fix a bug",
                            "fix: fix a bug

Tag: converter"
                        )
                        Git.Commit.Create(
                            "43c60e4fc9585a9f235ab6a6dd97c4c1cf945e46",
                            "feat: add a new feature",
                            "feat: add a new feature

Tag: cli"
                        )
                        Git.Commit.Create(
                            "34941a75efeeb3649c1adcec2c7f5c6257117a96",
                            "feat: make it do something",
                            "feat: make it do something

Tag: web"
                        )

                        Git.Commit.Create(
                            "c4bb772982b988db7d032263ae824bd2db653d6c",
                            "feat: make it do something"
                        )
                        Git.Commit.Create(
                            "b7eafe7744e4738d9578c09e1d128bbb2f5c40d3",
                            "feat: add another feature",
                            "feat: add another feature

Tag: cli"
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
                                            commits[0].RawBody
                                        |> Result.valueOr failwith
                                }
                                {
                                    OriginalCommit = commits[1]
                                    SemanticCommit =
                                        Parser.tryParseCommitMessage
                                            CommitParserConfig.Default
                                            commits[1].RawBody
                                        |> Result.valueOr failwith
                                }
                                {
                                    OriginalCommit = commits[4]
                                    SemanticCommit =
                                        Parser.tryParseCommitMessage
                                            CommitParserConfig.Default
                                            commits[4].RawBody
                                        |> Result.valueOr failwith
                                }
                            ]
                        LastCommitSha = "49c0699af98a67f1e8efcac8b1467b283a244aa8"
                    }
                    |> BumpRequired

                Expect.equal actual expected
            }
        ]

let private computeVersionTests =
    testList
        "computeVersionTests"
        [
            test "if previous version was a pre-release, release it as stable" {
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
                                    commits[0].RawBody
                                |> Result.valueOr failwith
                        }
                    ]

                let actual =
                    ReleaseContext.computeVersion
                        settings
                        commits
                        (SemVersion(2, 0, 0).WithPrereleaseParsedFrom("beta.1"))

                Expect.equal actual (Some(SemVersion(2, 0, 0)))
            }

            test
                "If user request a pre-release, should bump major and make start a pre-release if previous version was stable and changes include breaking change" {
                let settings =
                    GenerateSettings(
                        Changelog = "CHANGELOG.md",
                        PreRelease = FlagValue(Value = "beta", IsSet = true)
                    )

                let commits: Git.Commit list =
                    [
                        Git.Commit.Create(
                            "49c0699af98a67f1e8efcac8b1467b283a244aa8",
                            "fix!: fix a bug"
                        )
                    ]

                let commits: CommitForRelease list =
                    [
                        {
                            OriginalCommit = commits[0]
                            SemanticCommit =
                                Parser.tryParseCommitMessage
                                    CommitParserConfig.Default
                                    commits[0].RawBody
                                |> Result.valueOr failwith
                        }
                    ]

                let actual = ReleaseContext.computeVersion settings commits (SemVersion(2, 2, 45))

                Expect.equal actual (Some(SemVersion.ParsedFrom(3, 0, 0, "beta.1")))
            }

            test
                "If user request a pre-release, should bump minor and make start a pre-release if previous version was stable and changes include new features" {
                let settings =
                    GenerateSettings(
                        Changelog = "CHANGELOG.md",
                        PreRelease = FlagValue(Value = "beta", IsSet = true)
                    )

                let commits: Git.Commit list =
                    [
                        Git.Commit.Create(
                            "49c0699af98a67f1e8efcac8b1467b283a244aa8",
                            "fix: fix a bug"
                        )
                        Git.Commit.Create(
                            "49c0699af98a67f1e8efcac8b1467b283a244aa7",
                            "feat: add a new feature"
                        )
                    ]

                let commits: CommitForRelease list =
                    [
                        {
                            OriginalCommit = commits[0]
                            SemanticCommit =
                                Parser.tryParseCommitMessage
                                    CommitParserConfig.Default
                                    commits[0].RawBody
                                |> Result.valueOr failwith
                        }
                        {
                            OriginalCommit = commits[1]
                            SemanticCommit =
                                Parser.tryParseCommitMessage
                                    CommitParserConfig.Default
                                    commits[1].RawBody
                                |> Result.valueOr failwith
                        }
                    ]

                let actual = ReleaseContext.computeVersion settings commits (SemVersion(2, 2, 45))

                Expect.equal actual (Some(SemVersion.ParsedFrom(2, 3, 0, "beta.1")))
            }

            test
                "If user request a pre-release, should bump patch and make start a pre-release if previous version was stable and changes include only bug fixes" {
                let settings =
                    GenerateSettings(
                        Changelog = "CHANGELOG.md",
                        PreRelease = FlagValue(Value = "beta", IsSet = true)
                    )

                let commits: Git.Commit list =
                    [
                        Git.Commit.Create(
                            "49c0699af98a67f1e8efcac8b1467b283a244aa8",
                            "fix: fix a bug"
                        )
                        Git.Commit.Create(
                            "49c0699af98a67f1e8efcac8b1467b283a244aa7",
                            "fix: add a new feature"
                        )
                    ]

                let commits: CommitForRelease list =
                    [
                        {
                            OriginalCommit = commits[0]
                            SemanticCommit =
                                Parser.tryParseCommitMessage
                                    CommitParserConfig.Default
                                    commits[0].RawBody
                                |> Result.valueOr failwith
                        }
                        {
                            OriginalCommit = commits[1]
                            SemanticCommit =
                                Parser.tryParseCommitMessage
                                    CommitParserConfig.Default
                                    commits[1].RawBody
                                |> Result.valueOr failwith
                        }
                    ]

                let actual = ReleaseContext.computeVersion settings commits (SemVersion(2, 2, 45))

                Expect.equal actual (Some(SemVersion.ParsedFrom(2, 2, 46, "beta.1")))
            }

            test
                "If user request a pre-release, should increment the pre-release number if previous version was a pre-release (check for major version)" {
                let settings =
                    GenerateSettings(
                        Changelog = "CHANGELOG.md",
                        PreRelease = FlagValue(Value = "beta", IsSet = true)
                    )

                let commits: Git.Commit list =
                    [
                        Git.Commit.Create(
                            "49c0699af98a67f1e8efcac8b1467b283a244aa8",
                            "fix!: fix a bug"
                        )
                    ]

                let commits: CommitForRelease list =
                    [
                        {
                            OriginalCommit = commits[0]
                            SemanticCommit =
                                Parser.tryParseCommitMessage
                                    CommitParserConfig.Default
                                    commits[0].RawBody
                                |> Result.valueOr failwith
                        }
                    ]

                let actual =
                    ReleaseContext.computeVersion
                        settings
                        commits
                        (SemVersion.ParsedFrom(3, 0, 0, "beta.10"))

                Expect.equal actual (Some(SemVersion.ParsedFrom(3, 0, 0, "beta.11")))
            }

            test
                "If user request a pre-release, should increment the pre-release number if previous version was a pre-release (check for minor version)" {
                let settings =
                    GenerateSettings(
                        Changelog = "CHANGELOG.md",
                        PreRelease = FlagValue(Value = "beta", IsSet = true)
                    )

                let commits: Git.Commit list =
                    [
                        Git.Commit.Create(
                            "49c0699af98a67f1e8efcac8b1467b283a244aa8",
                            "fix: fix a bug"
                        )
                        Git.Commit.Create(
                            "49c0699af98a67f1e8efcac8b1467b283a244aa7",
                            "feat: add a new feature"
                        )
                    ]

                let commits: CommitForRelease list =
                    [
                        {
                            OriginalCommit = commits[0]
                            SemanticCommit =
                                Parser.tryParseCommitMessage
                                    CommitParserConfig.Default
                                    commits[0].RawBody
                                |> Result.valueOr failwith
                        }
                        {
                            OriginalCommit = commits[1]
                            SemanticCommit =
                                Parser.tryParseCommitMessage
                                    CommitParserConfig.Default
                                    commits[1].RawBody
                                |> Result.valueOr failwith
                        }
                    ]

                let actual =
                    ReleaseContext.computeVersion
                        settings
                        commits
                        (SemVersion.ParsedFrom(2, 2, 0, "beta.233"))

                Expect.equal actual (Some(SemVersion.ParsedFrom(2, 2, 0, "beta.234")))
            }

            test
                "If user request a pre-release, should increment the pre-release number if previous version was a pre-release (check for patch version)" {
                let settings =
                    GenerateSettings(
                        Changelog = "CHANGELOG.md",
                        PreRelease = FlagValue(Value = "beta", IsSet = true)
                    )

                let commits: Git.Commit list =
                    [
                        Git.Commit.Create(
                            "49c0699af98a67f1e8efcac8b1467b283a244aa8",
                            "fix: fix a bug"
                        )
                        Git.Commit.Create(
                            "49c0699af98a67f1e8efcac8b1467b283a244aa7",
                            "fix: add a new feature"
                        )
                    ]

                let commits: CommitForRelease list =
                    [
                        {
                            OriginalCommit = commits[0]
                            SemanticCommit =
                                Parser.tryParseCommitMessage
                                    CommitParserConfig.Default
                                    commits[0].RawBody
                                |> Result.valueOr failwith
                        }
                        {
                            OriginalCommit = commits[1]
                            SemanticCommit =
                                Parser.tryParseCommitMessage
                                    CommitParserConfig.Default
                                    commits[1].RawBody
                                |> Result.valueOr failwith
                        }
                    ]

                let actual =
                    ReleaseContext.computeVersion
                        settings
                        commits
                        (SemVersion.ParsedFrom(2, 2, 45, "beta.5"))

                Expect.equal actual (Some(SemVersion.ParsedFrom(2, 2, 45, "beta.6")))
            }

            test
                "If previous version was a pre-release, and user don't request a pre-release, release it as stable (check for major version)" {
                let settings = GenerateSettings(Changelog = "CHANGELOG.md")

                let commits: Git.Commit list =
                    [
                        Git.Commit.Create(
                            "49c0699af98a67f1e8efcac8b1467b283a244aa8",
                            "fix!: fix a bug"
                        )
                    ]

                let commits: CommitForRelease list =
                    [
                        {
                            OriginalCommit = commits[0]
                            SemanticCommit =
                                Parser.tryParseCommitMessage
                                    CommitParserConfig.Default
                                    commits[0].RawBody
                                |> Result.valueOr failwith
                        }
                    ]

                let actual =
                    ReleaseContext.computeVersion
                        settings
                        commits
                        (SemVersion(2, 0, 0).WithPrereleaseParsedFrom("beta.1"))

                Expect.equal actual (Some(SemVersion(2, 0, 0)))
            }

            test
                "If previous version was a pre-release, and user don't request a pre-release, release it as stable (check for minor version)" {
                let settings = GenerateSettings(Changelog = "CHANGELOG.md")

                let commits: Git.Commit list =
                    [
                        Git.Commit.Create(
                            "49c0699af98a67f1e8efcac8b1467b283a244aa8",
                            "feat: fix a bug"
                        )
                    ]

                let commits: CommitForRelease list =
                    [
                        {
                            OriginalCommit = commits[0]
                            SemanticCommit =
                                Parser.tryParseCommitMessage
                                    CommitParserConfig.Default
                                    commits[0].RawBody
                                |> Result.valueOr failwith
                        }
                    ]

                let actual =
                    ReleaseContext.computeVersion
                        settings
                        commits
                        (SemVersion(2, 0, 0).WithPrereleaseParsedFrom("beta.1"))

                Expect.equal actual (Some(SemVersion(2, 0, 0)))
            }

            test
                "If previous version was a pre-release, and user don't request a pre-release, release it as stable (check for patch version)" {
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
                                    commits[0].RawBody
                                |> Result.valueOr failwith
                        }
                    ]

                let actual =
                    ReleaseContext.computeVersion
                        settings
                        commits
                        (SemVersion(2, 0, 0).WithPrereleaseParsedFrom("beta.1"))

                Expect.equal actual (Some(SemVersion(2, 0, 0)))
            }

            test
                "If pre-release identifier is different start a new pre-release from 1 (check for major)" {
                let settings =
                    GenerateSettings(
                        Changelog = "CHANGELOG.md",
                        PreRelease = FlagValue(Value = "alpha", IsSet = true)
                    )

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
                                    commits[0].RawBody
                                |> Result.valueOr failwith
                        }
                    ]

                let actual =
                    ReleaseContext.computeVersion
                        settings
                        commits
                        (SemVersion(2, 0, 0).WithPrereleaseParsedFrom("beta.10"))

                Expect.equal actual (Some(SemVersion.ParsedFrom(2, 0, 0, "alpha.1")))
            }

            test
                "If pre-release identifier is different start a new pre-release from 1 (check for minor)" {
                let settings =
                    GenerateSettings(
                        Changelog = "CHANGELOG.md",
                        PreRelease = FlagValue(Value = "alpha", IsSet = true)
                    )

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
                                    commits[0].RawBody
                                |> Result.valueOr failwith
                        }
                    ]

                let actual =
                    ReleaseContext.computeVersion
                        settings
                        commits
                        (SemVersion(2, 2, 0).WithPrereleaseParsedFrom("beta.10"))

                Expect.equal actual (Some(SemVersion.ParsedFrom(2, 2, 0, "alpha.1")))
            }

            test
                "If pre-release identifier is different start a new pre-release from 1 (check for patch)" {
                let settings =
                    GenerateSettings(
                        Changelog = "CHANGELOG.md",
                        PreRelease = FlagValue(Value = "alpha", IsSet = true)
                    )

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
                                    commits[0].RawBody
                                |> Result.valueOr failwith
                        }
                    ]

                let actual =
                    ReleaseContext.computeVersion
                        settings
                        commits
                        (SemVersion(2, 2, 45).WithPrereleaseParsedFrom("beta.10"))

                Expect.equal actual (Some(SemVersion.ParsedFrom(2, 2, 45, "alpha.1")))
            }
        ]

let tests =
    testList
        "ReleaseContext"
        [
            computeTests
            computeVersionTests
        ]
