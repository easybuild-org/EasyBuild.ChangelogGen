module Log

open Spectre.Console

let private output =
    let settings = new AnsiConsoleSettings()
    settings.Out <- AnsiConsoleOutput(System.Console.Error)

    AnsiConsole.Create(settings)

let info msg =
    output.MarkupLine($"[deepskyblue3_1]%s{msg}[/]")

let success msg =
    output.MarkupLine($"[green]%s{msg}[/]")

let log msg =
    output.MarkupLine(msg)

let error msg =
    output.MarkupLine($"[red]{msg}[/]")

let warning msg =
    output.MarkupLine($"[yellow]{msg}[/]")
