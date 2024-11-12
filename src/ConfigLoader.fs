module EasyBuild.ChangelogGen.ConfigLoader

open EasyBuild.CommitParser.Types
open System.IO
open Thoth.Json.Core
open Thoth.Json.Newtonsoft
open EasyBuild.ChangelogGen.Types

type Config =
    {
        CommitParserConfig: CommitParserConfig
        ChangelogGenConfig: ChangelogGenConfig
    }

    static member Decoder: Decoder<Config> =
        Decode.object (fun get ->
            {
                CommitParserConfig = get.Required.Raw CommitParserConfig.decoder
                ChangelogGenConfig =
                    get.Optional.Field "changelog-gen" ChangelogGenConfig.Decoder
                    |> Option.defaultValue ChangelogGenConfig.Default
            }
        )

    static member Default =
        {
            CommitParserConfig = CommitParserConfig.Default
            ChangelogGenConfig = ChangelogGenConfig.Default
        }

let tryLoadConfig (cwd: string) (configFile: string option) : Result<Config, string> =
    let configFile =
        match configFile with
        | Some configFile -> Some <| new FileInfo(Path.Combine(cwd, configFile))
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
