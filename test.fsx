#r "nuget: SimpleExec, 13.0.0"
#r "nuget: BlackFox.CommandLine"
#r "nuget: Thoth.Json.System.Text.Json"

open SimpleExec
open BlackFox.CommandLine
open System.Threading.Tasks
open Thoth.Json.Core
open Thoth.Json.System.Text.Json

type System.Threading.Tasks.Task with

    static member inline RunSynchronously(task: System.Threading.Tasks.Task<'T>) : 'T =
        task.GetAwaiter().GetResult()

    static member inline RunSynchronously(task: System.Threading.Tasks.Task) : unit =
        task.GetAwaiter().GetResult()

// let struct (json, _) =
//     Command.ReadAsync(
//         "gh",
//         CmdLine.empty
//         |> CmdLine.appendRaw "auth"
//         |> CmdLine.appendRaw "status"
//         |> CmdLine.appendPrefix "--json" "hosts"
//         |> CmdLine.toString
//     )
//     |> Task.RunSynchronously

// let scopesDecoder =
//     Decode.string |> Decode.map (fun s -> s.Split ',' |> Array.toList)

// let decoder =
//     Decode.at
//         [
//             "hosts"
//             "github.com"
//         ]
//         (Decode.index 0 (Decode.field "scopes" scopesDecoder))

// let scopes = Decode.unsafeFromString decoder json

// printfn "%A" scopes

Command.Run(
    "gh",
    CmdLine.empty
    |> CmdLine.appendRaw "auth"
    |> CmdLine.appendRaw "status"
    |> CmdLine.appendPrefix "--json" "hosts"
    |> CmdLine.appendPrefix "--jq" ".hosts | add | add | .scopes"
    |> CmdLine.toString
)
