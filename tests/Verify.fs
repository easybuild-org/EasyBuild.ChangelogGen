module EasyBuild.ChangelogGen.Tests.Verify

open Expecto
open Tests.Utils
open EasyBuild.ChangelogGen.Generate
open EasyBuild.ChangelogGen.Generate.Types
open Spectre.Console.Cli
open System.IO

let tests =
    testList
        "Verify"
        [
            testList
                "options"
                [
                    test "Only one of --major, --minor, or --patch can be used at a time." {
                        let changelogInfo =
                            {
                                File = FileInfo(Path.GetTempFileName())
                                Content = ""
                                Versions = [ Semver.SemVersion(0, 0, 0) ]
                            }

                        Verify.options
                            (GenerateSettings(
                                BumpMajor = false,
                                BumpMinor = false,
                                BumpPatch = false
                            ))
                            changelogInfo
                        |> Expect.isOk

                        Verify.options (GenerateSettings(BumpMajor = true)) changelogInfo
                        |> Expect.isOk

                        Verify.options (GenerateSettings(BumpMinor = true)) changelogInfo
                        |> Expect.isOk

                        Verify.options (GenerateSettings(BumpPatch = true)) changelogInfo
                        |> Expect.isOk

                        Verify.options
                            (GenerateSettings(BumpMajor = true, BumpMinor = true))
                            changelogInfo
                        |> Expect.isError

                        Verify.options
                            (GenerateSettings(BumpMajor = true, BumpPatch = true))
                            changelogInfo
                        |> Expect.isError

                        Verify.options
                            (GenerateSettings(BumpMinor = true, BumpPatch = true))
                            changelogInfo
                        |> Expect.isError

                        Verify.options
                            (GenerateSettings(BumpMajor = true, BumpMinor = true, BumpPatch = true))
                            changelogInfo
                        |> Expect.isError
                    }

                    test "Cannot use --pre-release with --major, --minor, or --patch." {
                        let changelogInfo =
                            {
                                File = FileInfo(Path.GetTempFileName())
                                Content = ""
                                Versions = [ Semver.SemVersion(0, 0, 0) ]
                            }

                        Verify.options
                            (GenerateSettings(PreRelease = FlagValue(Value = "beta", IsSet = true)))
                            changelogInfo
                        |> Expect.isOk

                        Verify.options
                            (GenerateSettings(
                                BumpMajor = true,
                                PreRelease = FlagValue(Value = "beta", IsSet = true)
                            ))
                            changelogInfo
                        |> Expect.isError

                        Verify.options
                            (GenerateSettings(
                                BumpMinor = true,
                                PreRelease = FlagValue(Value = "beta", IsSet = true)
                            ))
                            changelogInfo
                        |> Expect.isError

                        Verify.options
                            (GenerateSettings(
                                BumpPatch = true,
                                PreRelease = FlagValue(Value = "beta", IsSet = true)
                            ))
                            changelogInfo
                        |> Expect.isError
                    }
                ]
        ]
