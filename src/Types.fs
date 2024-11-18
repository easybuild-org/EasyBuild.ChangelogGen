module EasyBuild.ChangelogGen.Types

open Thoth.Json.Core

type GithubRemoteConfig =
    {
        Owner: string
        Repository: string
    }

    static member Decoder: Decoder<GithubRemoteConfig> =
        Decode.object (fun get ->
            {
                Owner = get.Required.Field "owner" Decode.string
                Repository = get.Required.Field "repository" Decode.string
            }
        )
