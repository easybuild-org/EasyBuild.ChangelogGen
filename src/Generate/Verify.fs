module EasyBuild.ChangelogGen.Generate.Verify

open EasyBuild.ChangelogGen.Generate.Types
open EasyBuild.ChangelogGen

let branch (settings: GenerateSettings) =
    let currentBranchName = Git.getHeadBranchName ()

    if Array.contains currentBranchName settings.AllowBranch then
        Ok()
    else
        let allowedBranch =
            settings.AllowBranch |> Array.map (fun b -> $"- {b}") |> String.concat "\n"

        Error
            $"""Branch '%s{currentBranchName}' is not allowed to generate the changelog.

Allowed branches are:
%s{allowedBranch}

You can use the --allow-branch option to allow other branches."""

let dirty (settings: GenerateSettings) =
    if Git.isDirty () && not settings.AllowDirty then
        Error
            """Repository is dirty. Please commit or stash your changes before generating the changelog.

You can use the --allow-dirty option to allow a dirty repository."""
    else
        Ok()

let resolveRemoteConfig (config: ConfigLoader.Config) =
    match config.ChangelogGenConfig.Github with
    | Some _ -> Ok config
    | None ->
        match Git.tryFindRemote () with
        | Some remote ->
            Ok
                { config with
                    ChangelogGenConfig.Github =
                        Some
                            {
                                Owner = remote.Owner
                                Repository = remote.Repository
                            }
                }
        | None ->
            Error
                """Could not resolve the remote repository.

Automatic detection expected URL returned by `git config --get remote.origin.url` to be of the form 'https://hostname/owner/repo.git' or 'git@hostname:owner/repo.git'.

You can also provide the Github owner and repository in the configuration file:

```json
{
    "changelog-gen": {
        "github": {
            "owner": "owner",
            "repository": "repo"
        }
    }
}"""
