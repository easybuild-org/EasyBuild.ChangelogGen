module EasyBuild.ChangelogGen.Tests.Main

open Expecto

[<Tests>]
let allTests =
    testList "All Tests" [
        ReleaseContext.tests
        Git.tests
        Changelog.tests
    ]

[<EntryPoint>]
let main argv =
    runTestsWithCLIArgs [] Array.empty allTests
