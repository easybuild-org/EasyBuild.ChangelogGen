module EasyBuild.ChangelogGen.Tests.Main

open Fable.Pyxpecto

[<EntryPoint>]
let main argv =
    testList "All" [
        ChangelogTests.all
    ]
    |> Pyxpecto.runTests [||]
