[<AutoOpen>]
module Prelude

open Fable.Pyxpecto

type Assert =
    static member equal(actual: 'T, expected: 'T, ?message: string) =
        let message = defaultArg message ""

        Expect.equal actual expected message
