module Parser.Simple

open Parser.Base

[<RequireQualifiedAccess>]
type Problem =
    | Expecting of string
    | ExpectingEnd
    | UnexpectedChar


let toToken (str : string) =
    Token(str, Problem.Expecting str)
