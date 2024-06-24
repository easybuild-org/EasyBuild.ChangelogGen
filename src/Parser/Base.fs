module Parser.Base

open Parser.LowLevel

// Implementation notes:
// Row start at 1, column start at 1
//
// Example of row:
//
// 1 | This is the first line
// 2 | This is the second line
// 3 | This is the third line
//
// Example of column:
//
// 42 is the answer!
// ^
// 1
//
// 42 is the answer!
//  ^
//  2
//
// 42 is the answer!
//      ^
//      6

module String =

    let isEmpty (text: string) = text = ""

type LocatedContext<'Context> =
    {
        Row: int
        Column: int
        Context: 'Context
    }

    static member inline Create (row: int) (column: int) (context: 'Context) =
        {
            Row = row
            Column = column
            Context = context
        }

type State<'Context> =
    {
        Source: string
        Offset: int
        Indent: int
        Context: LocatedContext<'Context> list
        Row: int
        Column: int
    }

    static member inline Initial(source: string) =
        {
            Source = source
            Offset = 0
            Indent = 0
            Context = []
            Row = 1
            Column = 1
        }

type DeadEnd<'Context, 'Problem> =
    {
        Row: int
        Column: int
        Problem: 'Problem
        ContextStack: LocatedContext<'Context> list // Should it be a separated type?
    }

    /// <summary>
    /// Helper function making it easier to create a DeadEnd instance.
    ///
    /// This function does not impact the performance because it is inlined.
    /// </summary>
    /// <param name="row"></param>
    /// <param name="column"></param>
    /// <param name="problem"></param>
    /// <param name="context"></param>
    /// <typeparam name="'Context"></typeparam>
    /// <typeparam name="'Problem"></typeparam>
    /// <returns>
    /// A DeadEnd instance.
    /// </returns>
    static member inline Create
        (row: int)
        (column: int)
        (problem: 'Problem)
        (context: LocatedContext<'Context> list)
        =
        {
            Row = row
            Column = column
            Problem = problem
            ContextStack = context
        }

type Bag<'Context, 'Problem> =
    | Empty
    | AddRight of Bag<'Context, 'Problem> * DeadEnd<'Context, 'Problem>
    | Append of Bag<'Context, 'Problem> * Bag<'Context, 'Problem>

[<RequireQualifiedAccess>]
module ParserStep =

    type Success<'Context, 'Value> =
        {
            Backtrackable: bool
            Value: 'Value
            State: State<'Context>
        }

    type Failed<'Context, 'Problem> =
        {
            Backtrackable: bool
            Bag: Bag<'Context, 'Problem>
        }

[<RequireQualifiedAccess>]
type ParserStep<'Context, 'Problem, 'Value> =
    | Success of ParserStep.Success<'Context, 'Value>
    | Failed of ParserStep.Failed<'Context, 'Problem>

type Parser<'Context, 'Problem, 'Value> =
    | Parser of (State<'Context> -> ParserStep<'Context, 'Problem, 'Value>)

type Token<'T> = Token of string * 'T

let rec bagToList
    (bag: Bag<'Context, 'Problem>)
    (list: DeadEnd<'Context, 'Problem> list)
    : DeadEnd<'Context, 'Problem> list
    =
    match bag with
    | Empty -> list
    | AddRight(bag, deadEnd) -> bagToList bag (deadEnd :: list)
    | Append(bag1, bag2) -> bagToList bag1 (bagToList bag2 list)

let run
    (Parser parse: Parser<'Context, 'Problem, 'Value>)
    (src: string)
    : Result<'Value, DeadEnd<'Context, 'Problem> list>
    =
    let state =
        {
            Source = src
            Offset = 0
            Indent = 1
            Context = []
            Row = 1
            Column = 1
        }

    match parse state with
    | ParserStep.Success { Value = value } -> Ok value
    | ParserStep.Failed { Bag = bag } -> Error(bagToList bag [])

let fromState (state: State<'Context>) (problem: 'Problem) : Bag<'Context, 'Problem> =
    AddRight(Empty, DeadEnd.Create state.Row state.Column problem state.Context)

let fromInfo
    (row: int)
    (column: int)
    (problem: 'Problem)
    (context: LocatedContext<'Context> list)
    : Bag<'Context, 'Problem>
    =
    AddRight(Empty, DeadEnd.Create row column problem context)

let succeed (value: 'Value) =
    Parser
    <| fun state ->
        ParserStep.Success
            {
                Backtrackable = false
                Value = value
                State = state
            }

let problem (problem: 'Problem) : Parser<'Context, 'Problem, 'Value> =
    Parser
    <| fun state ->
        ParserStep.Failed
            {
                Backtrackable = false
                Bag = fromState state problem
            }

let map
    (func: 'A -> 'B)
    (Parser parse: Parser<'Context, 'Problem, 'A>)
    : Parser<'Context, 'Problem, 'B>
    =
    Parser
    <| fun state ->
        match parse state with
        | ParserStep.Success step ->
            ParserStep.Success
                {
                    Backtrackable = step.Backtrackable
                    Value = func step.Value
                    State = state
                }

        | ParserStep.Failed step ->
            ParserStep.Failed
                {
                    Backtrackable = step.Backtrackable
                    Bag = step.Bag
                }

let map2
    (func: 'A -> 'B -> 'Value)
    (Parser parseA: Parser<'Context, 'Problem, 'A>)
    (Parser parseB: Parser<'Context, 'Problem, 'B>)
    : Parser<'Context, 'Problem, 'Value>
    =
    Parser
    <| fun state ->
        match parseA state with
        | ParserStep.Failed stepA ->
            ParserStep.Failed
                {
                    Backtrackable = stepA.Backtrackable
                    Bag = stepA.Bag
                }

        | ParserStep.Success stepA ->
            match parseB stepA.State with
            | ParserStep.Failed stepB ->
                ParserStep.Failed
                    {
                        Backtrackable = stepA.Backtrackable || stepB.Backtrackable
                        Bag = stepB.Bag
                    }

            | ParserStep.Success stepB ->
                ParserStep.Success
                    {
                        Backtrackable = stepA.Backtrackable || stepB.Backtrackable
                        Value = func stepA.Value stepB.Value
                        State = stepB.State
                    }

let keeper
    (parseFunc: Parser<'Context, 'Problem, ('A -> 'B)>)
    (parseArg: Parser<'Context, 'Problem, 'A>)
    : Parser<'Context, 'Problem, 'B>
    =
    map2 (<|) parseFunc parseArg

let (|=) = keeper

let ignorer
    (keepParser: Parser<'Context, 'Problem, 'Keep>)
    (ignoreParser: Parser<'Context, 'Problem, 'Ignore>)
    : Parser<'Context, 'Problem, 'Keep>
    =
    map2 (fun keep _ -> keep) keepParser ignoreParser

let (|.) = ignorer

let skip
    (ignoreParser: Parser<'Context, 'Problem, 'Ignore>)
    (keeperParser: Parser<'Context, 'Problem, 'Keep>)
    : Parser<'Context, 'Problem, 'Keep>
    =
    map2 (fun _ b -> b) ignoreParser keeperParser

let andThen
    (func: 'A -> Parser<'Context, 'Problem, 'B>)
    (Parser parseA: Parser<'Context, 'Problem, 'A>)
    : Parser<'Context, 'Problem, 'B>
    =
    Parser
    <| fun state ->
        match parseA state with
        | ParserStep.Failed stepA ->
            ParserStep.Failed
                {
                    Backtrackable = stepA.Backtrackable
                    Bag = stepA.Bag
                }

        | ParserStep.Success stepA ->
            let (Parser parseB) = func stepA.Value

            match parseB stepA.State with
            | ParserStep.Failed stepB ->
                ParserStep.Failed
                    {
                        Backtrackable = stepA.Backtrackable || stepB.Backtrackable
                        Bag = stepB.Bag
                    }

            | ParserStep.Success stepB ->
                ParserStep.Success
                    {
                        Backtrackable = stepA.Backtrackable || stepB.Backtrackable
                        Value = stepB.Value
                        State = stepB.State
                    }

let lazy' (thunk: unit -> Parser<'Context, 'Problem, 'Value>) : Parser<'Context, 'Problem, 'Value> =
    Parser
    <| fun state ->
        let (Parser parse) = thunk ()
        parse state

let rec private oneOfApply
    (state: State<'Context>)
    (bag: Bag<'Context, 'Problem>)
    (parsers: Parser<'Context, 'Problem, 'Value> list)
    =
    match parsers with
    | [] ->
        ParserStep.Failed
            {
                Backtrackable = false
                Bag = bag
            }
    | Parser parse :: rest ->
        match parse state with
        | ParserStep.Success step -> ParserStep.Success step
        | ParserStep.Failed step ->
            if step.Backtrackable then
                ParserStep.Failed step
            else
                oneOfApply state (Append(bag, step.Bag)) rest

let oneOf (parsers: Parser<'Context, 'Problem, 'Value> list) : Parser<'Context, 'Problem, 'Value> =
    Parser <| fun state -> oneOfApply state Empty parsers

type LoopStep<'State, 'Value> =
    | Loop of 'State
    | Done of 'Value

let rec private loopApply
    (backtrackable: bool)
    (state: 'State)
    (func: 'State -> Parser<'Context, 'Problem, (LoopStep<'State, 'Value>)>)
    (state0: State<'Context>)
    : ParserStep<'Context, 'Problem, 'Value>
    =
    let (Parser parse) = func state

    match parse state0 with
    | ParserStep.Success step1 ->
        match step1.Value with
        | Loop newState ->
            loopApply (backtrackable || step1.Backtrackable) newState func step1.State
        | Done value ->
            ParserStep.Success
                {
                    Backtrackable = backtrackable || step1.Backtrackable
                    Value = value
                    State = step1.State
                }
    | ParserStep.Failed step1 ->
        ParserStep.Failed
            {
                Backtrackable = backtrackable || step1.Backtrackable
                Bag = step1.Bag
            }

let loop
    (state: 'State)
    (func: 'State -> Parser<'Context, 'Problem, (LoopStep<'State, 'Value>)>)
    : Parser<'Context, 'Problem, 'Value>
    =
    Parser <| fun state0 -> loopApply false state func state0

// TODO: Is the property `Backtrackable` named correctly?
// If yes, why do we set it to false in the `backtrackable` parser?
// I feel like property backtrackable should be named progress
let backtrackable
    (Parser parse: Parser<'Context, 'Problem, 'Value>)
    : Parser<'Context, 'Problem, 'Value>
    =
    Parser
    <| fun state ->
        match parse state with
        | ParserStep.Success step ->
            ParserStep.Success
                {
                    Backtrackable = false
                    Value = step.Value
                    State = step.State
                }

        | ParserStep.Failed step ->
            ParserStep.Failed
                {
                    Backtrackable = false
                    Bag = step.Bag
                }

let commit (value: 'Value) : Parser<'Context, 'Problem, 'Value> =
    Parser
    <| fun state ->
        ParserStep.Success
            {
                Backtrackable = true
                Value = value
                State = state
            }

let getPosition<'Context, 'Problem> : Parser<'Context, 'Problem, Position> =
    Parser
    <| fun state ->
        ParserStep.Success
            {
                Backtrackable = false
                Value =
                    {
                        Row = state.Row
                        Column = state.Column
                    }
                State = state
            }

let getRow<'Context, 'Problem> : Parser<'Context, 'Problem, int> =
    Parser
    <| fun state ->
        ParserStep.Success
            {
                Backtrackable = false
                Value = state.Row
                State = state
            }

let getColumn<'Context, 'Problem> : Parser<'Context, 'Problem, int> =
    Parser
    <| fun state ->
        ParserStep.Success
            {
                Backtrackable = false
                Value = state.Column
                State = state
            }

let getOffset<'Context, 'Problem> : Parser<'Context, 'Problem, int> =
    Parser
    <| fun state ->
        ParserStep.Success
            {
                Backtrackable = false
                Value = state.Offset
                State = state
            }

let getSource<'Context, 'Problem> : Parser<'Context, 'Problem, string> =
    Parser
    <| fun state ->
        ParserStep.Success
            {
                Backtrackable = false
                Value = state.Source
                State = state
            }

let getIndent<'Context, 'Problem> : Parser<'Context, 'Problem, int> =
    Parser
    <| fun state ->
        ParserStep.Success
            {
                Backtrackable = false
                Value = state.Indent
                State = state
            }

let changeIndent (newIndent: int) (state: State<'Context>) : State<'Context> =
    { state with Indent = newIndent }

let withIndent
    (newIndent: int)
    (Parser parse: Parser<'Context, 'Problem, 'Value>)
    : Parser<'Context, 'Problem, 'Value>
    =
    Parser
    <| fun state ->
        match parse (changeIndent newIndent state) with
        | ParserStep.Success step ->
            ParserStep.Success
                {
                    Backtrackable = step.Backtrackable
                    Value = step.Value
                    State = changeIndent state.Indent step.State
                }

        | ParserStep.Failed _ as failedStep -> failedStep

let changeContext
    (newContext: LocatedContext<'Context> list)
    (state: State<'Context>)
    : State<'Context>
    =
    { state with Context = newContext }

let inContext
    (context: 'Context)
    (Parser parse: Parser<'Context, 'Problem, 'Value>)
    : Parser<'Context, 'Problem, 'Value>
    =
    Parser
    <| fun state ->
        match
            parse (
                changeContext
                    (LocatedContext.Create state.Row state.Column context :: state.Context)
                    state
            )
        with
        | ParserStep.Success step ->
            ParserStep.Success
                {
                    Backtrackable = step.Backtrackable
                    Value = step.Value
                    State = changeContext state.Context step.State
                }

        | ParserStep.Failed _ as failedStep -> failedStep

let chompUntil (Token(str, expecting): Token<'Problem>) : Parser<'Context, 'Problem, unit> =
    Parser
    <| fun state ->
        let result = findSubString str state.Offset state.Row state.Column state.Source

        match result with
        | SubStringResult.NoMatch result ->
            ParserStep.Failed
                {
                    Backtrackable = false
                    Bag = fromInfo result.Row result.Column expecting state.Context
                }

        | SubStringResult.Match result ->
            ParserStep.Success
                {
                    Backtrackable = (state.Offset < result.Offset)
                    Value = ()
                    State =
                        { state with
                            Offset = result.Offset
                            Row = result.Row
                            Column = result.Column
                        }
                }

let chompIf (predicate: string -> bool) (expecting: 'Problem) : Parser<'Context, 'Problem, unit> =
    Parser
    <| fun state ->
        let newOffset = charMatchAt predicate state.Offset state.Source

        match newOffset with
        | CharMatchAtResult.NoMatch ->
            ParserStep.Failed
                {
                    Backtrackable = false
                    Bag = fromState state expecting
                }

        | CharMatchAtResult.NewLine ->
            ParserStep.Success
                {
                    Backtrackable = true
                    Value = ()
                    State =
                        { state with
                            Offset = state.Offset + 1
                            Row = state.Row + 1
                            Column = 1
                        }
                }

        | CharMatchAtResult.Match newOffset ->
            ParserStep.Success
                {
                    Backtrackable = true
                    Value = ()
                    State =
                        { state with
                            Offset = newOffset
                            Column = state.Column + 1
                        }
                }

let rec private chompWhileApply
    (predicate: string -> bool)
    (offset: int)
    (row: int)
    (col: int)
    (state: State<'Context>)
    : ParserStep<'Context, 'Problem, unit>
    =

    let newOffset = charMatchAt predicate offset state.Source

    match newOffset with
    | CharMatchAtResult.NoMatch ->
        ParserStep.Success
            {
                Backtrackable = (state.Offset < offset)
                Value = ()
                State =
                    { state with
                        Offset = offset
                        Row = row
                        Column = col
                    }
            }
    | CharMatchAtResult.NewLine -> chompWhileApply predicate (offset + 1) (row + 1) 1 state

    | CharMatchAtResult.Match newOffset -> chompWhileApply predicate newOffset row (col + 1) state

let chompWhile (predicate: string -> bool) : Parser<'Context, 'Problem, unit> =
    Parser
    <| fun state -> chompWhileApply predicate state.Offset state.Row state.Column state

let chumpUntilEndOr (str: string) : Parser<'Context, 'Problem, unit> =
    Parser
    <| fun state ->
        let result = findSubString str state.Offset state.Row state.Column state.Source

        // Important: chumpUntilEndOr always succeeds
        match result with
        | SubStringResult.NoMatch result ->
            ParserStep.Success
                {
                    // I think if we reach the end of the string, it means we made progress
                    Backtrackable = true
                    Value = ()
                    State =
                        { state with
                            Offset = state.Source.Length
                            Row = result.Row
                            Column = result.Column
                        }
                }
        | SubStringResult.Match result ->
            ParserStep.Success
                {
                    Backtrackable = state.Offset < result.Offset
                    Value = ()
                    State =
                        { state with
                            Offset = result.Offset
                            Row = result.Row
                            Column = result.Column
                        }
                }

let exhausted (problem: 'Problem) : Parser<'Context, 'Problem, unit> =
    Parser
    <| fun state ->
        if state.Source.Length = state.Offset then
            ParserStep.Success
                {
                    Backtrackable = false
                    Value = ()
                    State = state
                }
        else
            ParserStep.Failed
                {
                    Backtrackable = false
                    Bag = fromState state problem
                }

let end' = exhausted

/// <summary>
/// Updates the parser state with a new offset and adjusts the column number accordingly.
/// </summary>
/// <param name="newOffset">The new offset value to be set.</param>
/// <param name="state">The current state of the parser.</param>
/// <returns>
/// A new state object with the updated offset and column, while other parts of the state remain unchanged.</returns>
/// <remarks>
/// This function is particularly useful in text parsing scenarios where tracking the current position (offset and column) within the data is necessary.
/// It is a pure function that does not modify the input state but returns a new instance of the state with updated values.
/// </remarks>
let bumpOffset (newOffset: int) (state: State<'Context>) : State<'Context> =
    { state with
        Offset = newOffset
        Column = state.Column + (newOffset - state.Offset)
    }

let token (Token(str, expecting)) : Parser<'Context, 'Problem, unit> =
    Parser
    <| fun state ->
        let progress = not (String.isEmpty str)

        let result = findSubString str state.Offset state.Row state.Column state.Source

        match result with
        | SubStringResult.NoMatch _ ->
            ParserStep.Failed
                {
                    Backtrackable = false
                    Bag = fromState state expecting
                }

        | SubStringResult.Match result ->
            ParserStep.Success
                {
                    Backtrackable = progress
                    Value = ()
                    State =
                        { state with
                            Offset = result.Offset
                            Row = result.Row
                            Column = result.Column
                        }
                }

let mapChompedString
    (func: string -> 'A -> 'B)
    (Parser parse: Parser<'Context, 'Problem, 'A>)
    : Parser<'Context, 'Problem, 'B>
    =
    Parser
    <| fun state ->
        match parse state with
        | ParserStep.Failed step -> ParserStep.Failed step
        | ParserStep.Success step ->
            let chomped = state.Source.Substring(state.Offset, step.State.Offset - state.Offset)
            let value = func chomped step.Value

            ParserStep.Success
                {
                    Backtrackable = step.Backtrackable
                    Value = value
                    State = step.State
                }

let getChompedString (parser: Parser<'Context, 'Problem, 'A>) : Parser<'Context, 'Problem, string> =
    mapChompedString (fun chomped _ -> chomped) parser

let spaces<'Context, 'Problem> : Parser<'Context, 'Problem, unit> =
    chompWhile (fun c -> c = " " || c = "\r" || c = "\n")

let keyword (Token(keywordString, expecting)) : Parser<'Context, 'Problem, unit> =
    Parser
    <| fun state ->
        let progress = not (String.isEmpty keywordString)

        let result =
            isSubStringAt keywordString state.Offset state.Row state.Column state.Source

        match result with
        | IsSubStringAtResult.NoMatch ->
            ParserStep.Failed
                {
                    Backtrackable = false
                    Bag = fromState state expecting
                }

        | IsSubStringAtResult.Match cursorPosition ->
            ParserStep.Success
                {
                    Backtrackable = progress
                    Value = ()
                    State =
                        { state with
                            Offset = cursorPosition.Offset
                            Row = cursorPosition.Row
                            Column = cursorPosition.Column
                        }
                }
