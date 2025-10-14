module Log

open Spectre.Console

let private output =
    let settings = new AnsiConsoleSettings()
    settings.Out <- AnsiConsoleOutput(System.Console.Error)

    AnsiConsole.Create(settings)

let info msg =
    output.MarkupLine("[deepskyblue3_1]{0}[/]", Markup.Escape(msg))

let success msg =
    output.MarkupLine("[green]{0}[/]", Markup.Escape(msg))

let log msg = output.MarkupLine(msg)

let error msg =
    output.MarkupLine("[red]{0}[/]", Markup.Escape(msg))

let warning msg =
    output.MarkupLine("[yellow]{0}[/]", Markup.Escape(msg))
