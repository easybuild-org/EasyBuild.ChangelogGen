module EasyBuild.ChangelogGen.ConfigLoader

open EasyBuild.CommitParser.Types
open System.IO
open Thoth.Json.Core
open Thoth.Json.Newtonsoft
open EasyBuild.ChangelogGen.Types

type Config =
    {
        CommitParserConfig: CommitParserConfig
    }

    static member Decoder: Decoder<Config> =
        Decode.object (fun get ->
            { CommitParserConfig = get.Required.Raw CommitParserConfig.decoder }
        )

    static member Default = { CommitParserConfig = CommitParserConfig.Default }

let tryLoadConfig (configFile: string option) : Result<Config, string> =
    let configFile =
        match configFile with
        | Some configFile -> Some <| FileInfo configFile
        | None -> None

    match configFile with
    | Some configFile ->
        if not configFile.Exists then
            Error $"Configuration file '{configFile.FullName}' does not exist."
        else

            let configContent = File.ReadAllText(configFile.FullName)

            match Decode.fromString Config.Decoder configContent with
            | Ok config -> config |> Ok
            | Error error -> Error $"Failed to parse configuration file:\n\n{error}"

    | None -> Config.Default |> Ok
