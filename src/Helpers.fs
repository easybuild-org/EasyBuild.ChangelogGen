[<AutoOpen>]
module Helpers

module String =

    let normalizeNewLines (s: string) =
        s.Replace("\r\n", "\n").Replace("\r", "\n")

    let splitBy (newLineSeparator: char) (s: string) =
        s.Split(newLineSeparator) |> Array.toList
