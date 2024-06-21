module Log

open Spectre.Console

let info msg =
    AnsiConsole.MarkupLine($"[deepskyblue3_1]INFO:[/] {msg}")

let success msg =
    AnsiConsole.MarkupLine($"[green]SUCCESS:[/] {msg}")

let log msg =
    AnsiConsole.MarkupLine(msg)

let error msg =
    AnsiConsole.MarkupLine($"[red]ERROR:[/] {msg}")
