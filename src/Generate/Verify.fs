module EasyBuild.ChangelogGen.Generate.Verify

open EasyBuild.ChangelogGen.Generate.Types
open FsToolkit.ErrorHandling

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
