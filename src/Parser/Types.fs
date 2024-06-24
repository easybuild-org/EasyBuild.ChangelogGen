namespace Parser

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
