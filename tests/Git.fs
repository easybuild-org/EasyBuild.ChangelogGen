module EasyBuild.ChangelogGen.Tests.Git

open Expecto
open Tests.Utils

let tests =
    testList "Git" [
        // We can't test all the Git functions, because:
        // - getHeadBranchName: we can't garantee the branch name
        // - isDirty: we can't garantee the repository is clean
        // - getCommits(All): number of commits change over time
        // - getCommits(From): number of commits change over time
        //
        // But we still test some of the function to try minimize the risk of bugs and regressions

        test "getCommits to a specific SHA1 works" {
            let actual =
                Git.getCommits (Git.GetCommitsFilter.To "84fcad2edf2d47b1b799f83651dd8ab37ca84fc2")
                |> List.map _.Hash

            let expected = [
                "84fcad2edf2d47b1b799f83651dd8ab37ca84fc2"
                "41a35e6c856a6b6cd6aa9369f37ab1018cfc661e"
                "60dd6cf1f3746fcc70f832164127a5a7bb174f01"
                "a41069654c2c29ccb23d3de66185ba6190ab184f"
            ]

            Expect.equal actual expected
        }

        test "getCommits between SHA1 works" {
            let actual =
                Git.getCommits (Git.GetCommitsFilter.Between("60dd6cf1f3746fcc70f832164127a5a7bb174f01", "bd910a28dd883a9212de9ad24dee60e6c3b62f2d"))
                |> List.map _.Hash

            let expected = [
                "bd910a28dd883a9212de9ad24dee60e6c3b62f2d"
                "84fcad2edf2d47b1b799f83651dd8ab37ca84fc2"
                "41a35e6c856a6b6cd6aa9369f37ab1018cfc661e"
            ]

            Expect.equal actual expected
        }
    ]
