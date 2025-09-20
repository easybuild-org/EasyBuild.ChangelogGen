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
                                GenerateSettings(
                                    RemoteHostname = Some "gitlab.com",
                                    RemoteOwner = Some "owner",
                                    RemoteRepo = Some "repo"
                                )
                            )

                        let expected =
                            Ok(
                                {
                                    Hostname = "gitlab.com"
                                    Owner = "owner"
                                    Repository = "repo"
                                }
                                : RemoteConfig
                            )

                        Expect.equal expected actual
                    }

                    test "returns an error if not all `--remote-XXX` CLI arguments are provided" {
                        // Test some combinations of missing arguments
                        [
                            Verify.resolveRemoteConfig (
                                GenerateSettings(
                                    RemoteHostname = Some "gitlab.com",
                                    RemoteOwner = Some "owner"
                                )
                            )

                            Verify.resolveRemoteConfig (
                                GenerateSettings(
                                    RemoteHostname = Some "gitlab.com",
                                    RemoteRepo = Some "repo"
                                )
                            )

                            Verify.resolveRemoteConfig (
                                GenerateSettings(RemoteOwner = Some "owner")
                            )
                        ]
                        |> List.iter (fun actual ->
                            let expected =
                                Error
                                    """When using --remote-hostname, --remote-owner and --remote-repo they must be all provided."""

                            Expect.equal expected actual
                        )
                    }
                ]
        ]
