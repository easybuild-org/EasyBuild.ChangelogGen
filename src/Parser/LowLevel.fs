module Parser.LowLevel

open System.Text

type Position =
    {
        Row: int
        Column: int
    }

    static member inline Create (row: int) (column: int) =
        {
            Row = row
            Column = column
        }

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
    | NoMatch of noMatch: Position
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
let findSubString
    (searchedString: string)
    (offset: int)
    (row: int)
    (col: int)
    (text: string)
    : SubStringResult
    =
    let index = text.IndexOf(searchedString, offset)

    let target =
        if index = -1 then
            text.Length
        else
            index + searchedString.Length

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

    if index = -1 then
        SubStringResult.NoMatch(Position.Create row col)
    else
        SubStringResult.Match(CursorPosition.Create target row col)

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
let charMatchAt (predicate: string -> bool) (offset: int) (text: string) =
    if text.Length <= offset then
        CharMatchAtResult.NoMatch
    else
        let runeChar = Rune.GetRuneAt(text, offset)
        let charText = runeChar.ToString()

        if runeChar.Utf16SequenceLength = 2 then
            if predicate charText then
                CharMatchAtResult.Match(offset + 2)
            else
                CharMatchAtResult.NoMatch
        else if predicate charText then
            if runeChar.Value = 10 then
                CharMatchAtResult.NewLine
            else
                CharMatchAtResult.Match(offset + 1)
        else
            CharMatchAtResult.NoMatch

[<Struct>]
[<RequireQualifiedAccess>]
type IsSubStringAtResult =
    | NoMatch
    | Match of CursorPosition

let isSubStringAt (searchedString: string) (offset: int) (row: int) (col: int) (text: string) =

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
        IsSubStringAtResult.Match(CursorPosition.Create offset row col)
    else
        IsSubStringAtResult.NoMatch

let eatBase10 (offset : int) (text : string) =
    let mutable offset = offset
    let mutable isGood = true

    while isGood && offset < text.Length do
        let rune = Rune.GetRuneAt(text, offset)
        isGood <- rune.Value >= 48 && rune.Value <= 57
        // Only increment if the rune is a digit
        if isGood then
            offset <- offset + rune.Utf16SequenceLength

    offset
