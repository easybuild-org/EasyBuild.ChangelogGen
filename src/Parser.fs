module Parser

open System
open Fable.Core
open Fable.Core.JsInterop
open System.Text

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

type LocatedContext<'Context> =
    {
        Row: int
        Column: int
        Context: 'Context
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

let rec bagToList
    (bag: Bag<'Context, 'Problem>)
    (list: DeadEnd<'Context, 'Problem> list)
    : DeadEnd<'Context, 'Problem> list
    =
    match bag with
    | Empty -> list
    | AddRight(bag, deadEnd) -> bagToList bag (deadEnd :: list)
    | Append(bag1, bag2) -> bagToList bag1 (bagToList bag2 list)

let run (Parser parse) (src: string) =
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

let ignorer
    (keepParser: Parser<'Context, 'Problem, 'Keep>)
    (ignoreParser: Parser<'Context, 'Problem, 'Ignore>)
    : Parser<'Context, 'Problem, 'Keep>
    =
    map2 (fun keep _ -> keep) keepParser ignoreParser

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

let rec internal oneOfApply
    (state: State<'Context>)
    (bag: Bag<'Context, 'Problem>)
    (parsers: Parser<'Context, 'Problem, 'Value> list)
    =
    match parsers with
    | [] -> ParserStep.Failed { Backtrackable = false; Bag = bag }
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

type Step<'State, 'Value> =
    | Loop of 'State
    | Done of 'Value

let rec internal loopApply
    (backtrackable: bool)
    (state: 'State)
    (func: 'State -> Parser<'Context, 'Problem, (Step<'State, 'Value>)>)
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

let backstractable
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

type Token<'T> = Token of string * 'T

// let isSubString (smallString: string) (offset: int) (row: int) (col: int) (bigString: string) =
//     let smallLength = smallString.Length
//     let mutable isGood = offset + smallLength <= bigString.Length
//     let mutable i = 0
//     let mutable offset = offset
//     let mutable row = row
//     let mutable col = col

//     while (isGood && i < smallLength) do
//         let code = charCodeAt bigString offset
//         // Increment counters
//         i <- i + 1
//         offset <- offset + 1
//         let condA = smallString[i] = bigString[offset]
//         let isNewLine = code = 0x000A
//         let x =
//             if isNewLine then
//                 (row + 1, 1)
//             else

//         isGood <-

//     ()

//

// let rec loop i =
//     if not isGood || i >= smallLength then
//         (if isGood then offset else (-1, row, col))
//     else
//         let code = int (bigString.[offset])
//         let isGood' =
//             smallString.[i] = bigString.[offset] &&
//             (
//                 if code = 0x000A then
//                     row + 1, 1
//                 else
//                     col + 1, if (code &&& 0xF800) = 0xD800 then
//                                 i + 1
//                                 offset + 1
//                             else
//                                 i
//                                 offset
//             )
//         loop (i + 1) isGood' row col

// loop 0

// let token (Token (str, expecting) : Token<'Problem>) : Parser<'Context, 'Problem, unit> =
//     let progress =
//         str <> ""

//     Parser
//     <| fun state ->
//         let

// let symbol (x : Token<'Problem>) : Parser<'Context, 'Problem, unit> =
//     Token x

let inline private isTwoBytesUtf16Characters (charCode: int) =
    // Is this the right way to detect UTF-16 characters that take 2 bytes?
    charCode >= 0xD800 && charCode <= 0xDBFF

// let bytes = Encoding.Unicode.GetBytes(string code)
// Array.length bytes = 2 && bytes.[1] = 255uy

// // https://learn.microsoft.com/en-us/dotnet/api/system.text.encoding.getpreamble?view=net-8.0
// let rune = char.EnumerateRunes()

// rune.Current.Utf16SequenceLength = 2

type CursorPosition =
    {
        Offset: int
        Row: int
        Column: int
    }

    static member inline Create (offset: int) (row: int) (col: int) =
        {
            Offset = offset
            Row = row
            Column = col
        }


[<Struct>]
[<RequireQualifiedAccess>]
type SubStringResult =
    | NoMatch
    | Match of CursorPosition

/// <summary>
/// Find a substring after a given offset.
/// </summary>
/// <param name="searchedString">The string to search for</param>
/// <param name="offset">The offset to start searching from</param>
/// <param name="row">Initial row</param>
/// <param name="col">Initial column</param>
/// <param name="text">The text to search in</param>
/// <returns>
/// The returned <c>column</c> is the column right after the found substring.
/// The returned <c>row</c> is the row where the found substring is.
///
/// For <c>findSubString "42" 0 1 1 "42 is the answer!"</c> the result is <c>{ Offset = 0; Row = 1; Column = 3 }</c>.
/// For <c>findSubString "answer!" 0 1 1 "42 is the answer!"</c> the result is <c>{ Offset = 10; Row = 1; Column = 18 }</c>.
/// </returns>
let findSubString (searchedString: string) (offset: int) (row: int) (col: int) (text: string) : SubStringResult =
    let newOffset = text.IndexOf(searchedString, offset)

    let target =
        if newOffset = -1 then
            text.Length
        else
            newOffset + searchedString.Length

    if newOffset = -1 then
        SubStringResult.NoMatch
    else
        // Memory
        let mutable offset = offset
        let mutable row = row
        let mutable col = col

        let newText = text.Substring(offset)
        let mutable iterator = newText.EnumerateRunes()

        while offset < target do
            let rune = iterator.Current
            offset <- offset + 1

            if rune.Value = 10 then // '\n'
                row <- row + 1
                col <- 2
            else if rune.Utf16SequenceLength <> 2 then
                col <- col + 1

            iterator.MoveNext() |> ignore

        SubStringResult.Match (CursorPosition.Create newOffset row col)


/// <summary>
/// Checks if the character at the specified offset in the text is an ASCII character
/// and equals the given character code.
/// </summary>
/// <param name="charCode">The character code to compare.</param>
/// <param name="offset">The offset of the character in the text.</param>
/// <param name="text">The text to check.</param>
/// <returns>
/// True if the character at the specified offset in the text is an ASCII character
/// and the character code matches the ASCII code of the character at the specified offset in the text, otherwise false.
/// </returns>
let isAsciiCode (charCode: int) (offset: int) (text: string) =
    let charText = text.[offset]

    (string charText).EnumerateRunes().Current.IsAscii && charCode = int charText

// Is using a DUs Struct a good idea?
// In general, others parsers use `int` directly but this hurts readability of the code.
// By using, a DU Struct I hope to make the code more readable and still keep the performance.
[<Struct>]
[<RequireQualifiedAccessAttribute>]
type CharMatchAtResult =
    /// <summary>
    /// The character at the specified offset in the text did not match the character code.
    /// </summary>
    | NoMatch
    /// <summary>
    /// The character at the specified offset in the text matched the character code
    /// and was a new line character.
    /// </summary>
    | NewLine
    /// <summary>
    /// The character at the specified offset in the text matched the character code
    /// </summary>
    | Match of int

/// <summary>
/// Checks if the character at the specified offset in the text matches the given predicate.
///
/// Important: We are using <c>string -> bool</c> instead of <c>char -> bool</c> otherwise we
/// can't work with UTF-16 characters / emojis.
///
/// In the future, we should probably use <c>Rune</c> instead of <c>string</c>.
/// But Fable, doesn't support <c>Rune</c> yet.
/// </summary>
/// <param name="predicate">The predicate to check if the character matches.</param>
/// <param name="offset">The offset of the character in the text.</param>
/// <param name="text">The text to check.</param>
/// <returns>
/// - <c>NoMatch</c> if the character at the specified offset in the text did not match the character code.
/// - <c>NewLine</c> if the character at the specified offset in the text matched the character code and was a new line character.
/// - <c>Match newOffset</c> if the character at the specified offset in the text matched the character code.
///
///     The <c>newOffset</c> is the offset of the next character in the text.
///
///     It is <c>offset + 1</c> if the character is encoded on 1 byte in UTF-16 and <c>offset + 2</c> if the character is encoded on 2 bytes in UTF-16.
/// </returns>
let charMatchAt (predicate : string -> bool) (offset: int) (text: string) =
    if text.Length <= offset then
        CharMatchAtResult.NoMatch
    else
        let runeChar = Rune.GetRuneAt(text, offset)
        let charText = runeChar.ToString()

        if runeChar.Utf16SequenceLength = 2 then
            if predicate charText then
                CharMatchAtResult.Match (offset + 2)
            else
                CharMatchAtResult.NoMatch
        else
            if predicate charText then
                if runeChar.Value = 10 then
                    CharMatchAtResult.NewLine
                else
                    CharMatchAtResult.Match (offset + 1)
            else
                CharMatchAtResult.NoMatch

[<Struct>]
[<RequireQualifiedAccess>]
type IsSubStringAtResult =
    | NoMatch
    | Match of CursorPosition

let isSubStringAt
    (searchedString: string)
    (offset: int)
    (row: int)
    (col: int)
    (text: string) =

    let searchedStringLength = searchedString.Length

    // Memory
    let mutable isGood = offset + searchedStringLength <= text.Length
    // Lookup index in the searched string
    let mutable i = 0
    // Lookup offset position in the text
    let mutable offset = offset
    let mutable row = row
    let mutable col = col

    while isGood && i < searchedStringLength do
        let rune = Rune.GetRuneAt(text, offset)

        if rune.Value = 10 then // '\n'
            row <- row + 1
            col <- 1
        else
            col <- col + 1

        isGood <- Rune.GetRuneAt(searchedString, i) = rune

        if isGood then
            i <- i + rune.Utf16SequenceLength
            offset <- offset + rune.Utf16SequenceLength

    if isGood then
        IsSubStringAtResult.Match (CursorPosition.Create offset row col)
    else
        IsSubStringAtResult.NoMatch
