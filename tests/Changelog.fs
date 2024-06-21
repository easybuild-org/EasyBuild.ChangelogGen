module EasyBuild.ChangelogGen.Tests.ChangelogTests

open Fable.Pyxpecto
open EasyBuild.ChangelogGen
open EasyBuild.ChangelogGen.Changelog
open Semver
open System

// [<Test>]
// let ``Empty FablePackageType property should report an error`` () =
//     let content =
//         """# Changelog

// All notable changes to this project will be documented in this file.

// This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

// This changelog is generated using [EasyBuild.ChangelogGen](https://github.com/easybuild-org/EasyBuild.ChangelogGen). Do not edit this file manually.

// <!-- EasyBuild: START -->
// <!-- EasyBuild: END -->

// ## Unreleased

// ### Added

// - Add support for Fable 3.0.0

// ## 0.1.0 - 2021-08-01

// ### Added

// - Add support for Fable 1.0.0

// ### Fixed

// - Fix issue with Fable 1.0.0
//     """

//     let expected: Changelog =
//         {
//             Title = Some "Changelog"
//             Description =
//                 Some
//                     """All notable changes to this project will be documented in this file.

// This project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

// This changelog is generated using [EasyBuild.ChangelogGen](https://github.com/easybuild-org/EasyBuild.ChangelogGen). Do not edit this file manually."""
//             Metadata = None
//             Unreleased =
//                 {
//                     Version = SemVersion(0, 1, 0)
//                     Date = Some(DateOnly(2021, 8, 1))
//                     Body =
//                         """### Added

// - Add support for Fable 3.0.0"""
//                 }
//                 |> Some
//             Versions =
//                 [
//                     {
//                         Version = SemVersion(0, 1, 0)
//                         Date = Some(DateOnly(2021, 8, 1))
//                         Body =
//                             """### Added

// - Add support for Fable 1.0.0

// ### Fixed

// - Fix issue with Fable 1.0.0"""
//                     }
//                 ]
//         }

//     Assert.That(parse content, Is.EqualTo expected)

type Assert =
    static member equal (actual : 'T, expected : 'T, ?message : string) =
        let message = defaultArg message ""

        Expect.equal actual expected message

let all =
    testList "Parser" [
        testList "FindSubString" [

            testCase "works when the searched string is at the beginning" (fun () ->
                let actual = Parser.findSubString "42" 0 1 1 "42 is the answer!"

                Assert.equal(actual, (Parser.SubStringResult.Create 0 1 3))
            )

            testCase "works when the searched string is at the end" (fun () ->
                let actual = Parser.findSubString "answer!" 0 1 1 "42 is the answer!"

                Assert.equal(actual, Parser.SubStringResult.Create 10 1 18)
            )

            testCase "works when the searched string is in the middle" (fun () ->
                let actual = Parser.findSubString "is" 0 1 1 "42 is the answer!"

                Assert.equal(actual, Parser.SubStringResult.Create 3 1 6)
            )

            testCase "works when the searched string is at the beginning of a new line" (fun () ->
                let actual = Parser.findSubString "42" 0 1 1 "Is \n\n\n42\nthe answer?"

                Assert.equal(actual, Parser.SubStringResult.Create 6 4 3)
            )

            testCase "make sure the same column is returned relativy to row positio" (fun () ->
                let actual = Parser.findSubString "42" 0 1 1 "Is 42 the answer?"

                Assert.equal(actual, Parser.SubStringResult.Create 3 1 6)

                let actual = Parser.findSubString "42" 0 1 1 "\nIs 42 the answer?"

                Assert.equal(actual, Parser.SubStringResult.Create 4 2 6)
            )

            testCase "works with unicode taking 2 bytes" (fun () ->
                let actual = Parser.findSubString "👍" 0 1 1 "Great 👍"

                Assert.equal(actual, Parser.SubStringResult.Create 6 1 8)

                let actual = Parser.findSubString "👍" 0 1 1 "This is a 👍 great emoji"

                Assert.equal(actual, Parser.SubStringResult.Create 10 1 12)

                let actual = Parser.findSubString "👍" 0 1 1 "🚀 This is a 👍 great emoji"

                Assert.equal(actual, Parser.SubStringResult.Create 13 1 14)
            )
        ]
    ]
