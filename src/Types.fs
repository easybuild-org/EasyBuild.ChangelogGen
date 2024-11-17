module EasyBuild.ChangelogGen.Types

open Thoth.Json.Core

type BumpRule =
    | Major
    | Minor
    | Patch

    static member Decoder: Decoder<BumpRule> =
        Decode.string
        |> Decode.andThen (
            function
            | "major" -> Decode.succeed Major
            | "minor" -> Decode.succeed Minor
            | "patch" -> Decode.succeed Patch
            | invalid ->
                Decode.fail $"Invalid bump rule: {invalid}, expected 'major', 'minor' or 'patch'."
        )

type Groups =
    {
        Type: string
        Bump: BumpRule
        Group: string
    }

    static member Decoder: Decoder<Groups> =
        Decode.object (fun get ->
            {
                Type = get.Required.Field "type" Decode.string
                Bump = get.Required.Field "bump" BumpRule.Decoder
                Group = get.Required.Field "group" Decode.string
            }
        )

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

type ChangelogGenConfig =
    {
        Github: GithubRemoteConfig option
        Groups: Groups list
    }

    static member Decoder: Decoder<ChangelogGenConfig> =
        Decode.object (fun get ->
            {
                Github = get.Optional.Field "github" GithubRemoteConfig.Decoder
                Groups = get.Required.Field "groups" (Decode.list Groups.Decoder)
            }
        )

    static member Default =
        {
            Github = None
            Groups =
                [
                    {
                        // Special group name for breaking changes
                        // We can do this because group name can't have spaces
                        Type = "breaking change"
                        Bump = Major
                        Group = "🏗️ Breaking changes"
                    }
                    {
                        Type = "feat"
                        Bump = Minor
                        Group = "🚀 Features"
                    }
                    {
                        Type = "fix"
                        Bump = Patch
                        Group = "🐞 Bug Fixes"
                    }
                ]
        }
