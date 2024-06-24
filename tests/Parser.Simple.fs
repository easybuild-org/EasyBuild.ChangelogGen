module EasyBuild.ChangelogGen.Tests.Parser.Simple

open Fable.Pyxpecto
open Parser

module Parser =

    let boolean =
        Parser.oneOf
            [
                Parser.succeed true
                |. (Parser.keyword "true")

                Parser.succeed false
                |. (Parser.keyword "false")
            ]

let tests =
    testList
        "Parser.Simple"
        [
            testCase "Parser.boolean works with 'false'"
            <| fun _ ->
                let result = Parser.run Parser.boolean "false"

                Assert.equal (result, Ok false)

            testCase "Parser.boolean works with 'true'"
            <| fun _ ->
                let result = Parser.run Parser.boolean "true"

                Assert.equal (result, Ok true)

            testCase "Parser.boolean fails with 'tru'"
            <| fun _ ->
                let result = Parser.run Parser.boolean "tru"

                Assert.equal (
                    result,
                    Error
                        [
                            {
                                Row = 1
                                Column = 1
                                Problem = Problem.Expecting "true"
                            }
                            {
                                Row = 1
                                Column = 1
                                Problem = Problem.Expecting "false"
                            }
                        ]
                )
        ]
