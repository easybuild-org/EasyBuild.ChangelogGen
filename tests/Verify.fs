module EasyBuild.ChangelogGen.Tests.Verify

open Expecto
open Tests.Utils
open EasyBuild.ChangelogGen.Generate
open EasyBuild.ChangelogGen.Generate.Types
open EasyBuild.ChangelogGen.Types

let tests =
    testList
        "Verify"
        [
            testList
                "resolveRemoteConfig"
                [
                    test "resolveRemoteConfig priorize CLI arguments" {
                        let actual =
                            Verify.resolveRemoteConfig (
                                GenerateSettings(GitHubRepo = Some "owner/repo")
                            )

                        let expected =
                            Ok(
                                {
                                    Owner = "owner"
                                    Repository = "repo"
                                }
                                : GithubRemoteConfig
                            )

                        Expect.equal expected actual
                    }

                    test "returns an error if CLI argument is not in the right format" {
                        let actual =
                            Verify.resolveRemoteConfig (GenerateSettings(GitHubRepo = Some "owner"))

                        let expected =
                            Error
                                "Invalid format for --github-repo option, expected format is 'owner/repo'."

                        Expect.equal expected actual
                    }
                ]
        ]
