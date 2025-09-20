module EasyBuild.ChangelogGen.Tests.Git

open Expecto
open Tests.Utils

let tests =
    testList
        "Git"
        [
            // We can't test all the Git functions, because:
            // - getHeadBranchName: we can't garantee the branch name
            // - isDirty: we can't garantee the repository is clean
            // - getCommits(All): number of commits change over time
            // - getCommits(From): number of commits change over time
            //
            // But we still test some of the function to try minimize the risk of bugs and regressions

            test "readCommit works" {
                let actual = Git.readCommit "5e74c5f6ccd3ef71bbfc58bae943333460ad13ee"

                let expected: Git.Commit =
                    {
                        Hash = "5e74c5f6ccd3ef71bbfc58bae943333460ad13ee"
                        AbbrevHash = "5e74c5f"
                        Author = "Maxime Mangel"
                        ShortMessage =
                            "chore: Move test to Fable.Pyxpecto to prepare for Fable support in the future + and more low level Api"
                        RawBody =
                            "chore: Move test to Fable.Pyxpecto to prepare for Fable support in the future + and more low level Api
"
                    }

                Expect.equal actual expected
            }

            testList
                "tryGetRemoteFromUrl"
                [
                    test "works with https" {
                        let actual = Git.tryGetRemoteFromUrl "https://github.com/owner/repo.git"

                        let expected: Git.Remote =
                            {
                                Hostname = "github.com"
                                Owner = "owner"
                                Repository = "repo"
                            }

                        Expect.equal actual (Some expected)
                    }

                    test "works even without .git suffix" {
                        let actual = Git.tryGetRemoteFromUrl "https://github.com/owner/repo"

                        let expected: Git.Remote =
                            {
                                Hostname = "github.com"
                                Owner = "owner"
                                Repository = "repo"
                            }

                        Expect.equal actual (Some expected)
                    }

                    test "returns None when url is invalid" {
                        let actual = Git.tryGetRemoteFromUrl "https://github.com/missing-segments"

                        Expect.equal actual None
                    }
                ]

            testList
                "tryGetRemoteFromSSH"
                [
                    test "works with ssh" {
                        let actual = Git.tryGetRemoteFromSSH "git@github.com:owner/repo.git"

                        let expected: Git.Remote =
                            {
                                Hostname = "github.com"
                                Owner = "owner"
                                Repository = "repo"
                            }

                        Expect.equal actual (Some expected)
                    }

                    test "works even without .git suffix" {
                        let actual = Git.tryGetRemoteFromSSH "git@github.com:owner/repo"

                        let expected: Git.Remote =
                            {
                                Hostname = "github.com"
                                Owner = "owner"
                                Repository = "repo"
                            }

                        Expect.equal actual (Some expected)
                    }

                    test "returns None when url is invalid" {
                        Expect.equal (Git.tryGetRemoteFromSSH "github.com:owner/repo.git") None
                        Expect.equal (Git.tryGetRemoteFromSSH "git@github.comowner/repo.git") None
                        Expect.equal (Git.tryGetRemoteFromSSH "git@github.com:ownerrepo.git") None
                    }

                ]
        ]
