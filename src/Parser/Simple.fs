namespace Parser

open Parser

[<RequireQualifiedAccess>]
type Problem =
    | Expecting of string
    | ExpectingEnd
    | UnexpectedChar

type DeadEnd =
    {
        Row: int
        Column: int
        Problem: Problem
    }

// TODO: What should be the type of 'Context ?
type Parser<'Value> = Base.Parser<obj, Problem, 'Value>

module Parser =

    let succeed: 'Value -> Parser<'Value> = Base.succeed

    let run (parser: Parser<'Value>) (text: string) : Result<'Value, DeadEnd list> =
        match Base.run parser text with
        | Ok value -> Ok value
        | Error problems ->
            problems
            |> List.map (fun problem ->
                {
                    Row = problem.Row
                    Column = problem.Column
                    Problem = problem.Problem
                }
            )
            |> Error

    let deadEndsToString (deadEnds: DeadEnd list) : string =
        deadEnds
        |> List.map (fun deadEnd ->
            let problemMsg =
                match deadEnd.Problem with
                | Problem.Expecting str -> $"Expecting %s{str}"
                | Problem.ExpectingEnd -> "Expecting end"
                | Problem.UnexpectedChar -> "Unexpected char"

            $"({deadEnd.Row}, {deadEnd.Column}): {problemMsg}"
        )
        |> String.concat "\n"

    let keeper = Base.keeper

    let ignorer = Base.ignorer

    let skip (ignoreParser: Parser<'Value>) (parser: Parser<'Value>) : Parser<'Value> =
        Base.skip ignoreParser parser

    let lazy' (thunk: (unit -> Parser<'Value>)) : Parser<'Value> = Base.lazy' thunk

    let andThen (func: 'A -> Parser<'B>) (parser: Parser<'A>) : Parser<'B> = Base.andThen func parser

    let problem (problem: Problem) : Parser<'Value> = Base.problem problem

    let oneOf (parsers: Parser<'Value> list) : Parser<'Value> = Base.oneOf parsers

    let map (func: 'A -> 'B) (parser: Parser<'A>) : Parser<'B> = Base.map func parser

    let map2 (func: 'A -> 'B -> 'C) (parserA: Parser<'A>) (parserB: Parser<'B>) : Parser<'C> =
        Base.map2 func parserA parserB

    let backtrackable (parser: Parser<'Value>) : Parser<'Value> = Base.backtrackable parser

    let commit (value: 'Value) : Parser<'Value> = Base.commit value

    let toToken (str: string) = Base.Token(str, Problem.Expecting str)

    let token (str: string) : Parser<unit> = Base.token (toToken str)

    type LoopStep<'State, 'Value> =
        | Loop of 'State
        | Done of 'Value

    let private toBaseLoopStep (step: LoopStep<'State, 'Value>) =
        match step with
        | Loop state -> Base.Loop state
        | Done value -> Base.Done value

    let loop (state: 'State) (func: 'State -> Parser<LoopStep<'State, 'Value>>) : Parser<'Value> =
        Base.loop state (fun state -> map toBaseLoopStep (func state))

    let exhausted: Parser<unit> = Base.exhausted Problem.ExpectingEnd

    let end': Parser<unit> = Base.end' Problem.ExpectingEnd

    let mapChompedString (func: string -> string -> 'Value) (parser: Parser<string>) : Parser<'Value> =
        Base.mapChompedString func parser

    let getChompedString (parser: Parser<string>) : Parser<string> = Base.getChompedString parser

    let chompIf (predicate: string -> bool) : Parser<unit> =
        Base.chompIf predicate Problem.UnexpectedChar

    let chompWhile (predicate: string -> bool) : Parser<unit> = Base.chompWhile predicate

    let chompUntil (str: string) : Parser<unit> = Base.chompUntil (toToken str)

    let chumpUntilEndOr (str: string) : Parser<unit> = Base.chumpUntilEndOr str

    let getPosition : Parser<Position> = Base.getPosition

    let getRow : Parser<int> = Base.getRow

    let getColumn : Parser<int> = Base.getColumn

    let getOffset : Parser<int> = Base.getOffset

    let getSource : Parser<string> = Base.getSource

    let getIndent : Parser<int> = Base.getIndent

    let withIndent (newIndent: int) (parser: Parser<'Value>) : Parser<'Value> =
        Base.withIndent newIndent parser

    let spaces : Parser<unit> = Base.spaces

    let keyword (str: string) : Parser<unit> = Base.keyword (toToken str)

[<AutoOpen>]
module Operators =

    let (|=) = Parser.keeper

    let (|.) = Parser.ignorer
