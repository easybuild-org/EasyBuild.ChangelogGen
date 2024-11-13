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

let options (settings: GenerateSettings) (changelog : ChangelogInfo) =
    let bumpOptions =
        match settings.BumpMajor, settings.BumpMinor, settings.BumpPatch with
        | true, false, false
        | false, true, false
        | false, false, true ->
            if changelog.LastVersion.IsPrerelease then
                Error "Previous version is a pre-release version. Cannot bump major, minor, or patch version."
            else
                Ok ()
        | false, false, false -> Ok ()
        | _ -> Error "Only one of --major, --minor, or --patch can be used at a time."

    let preReleaseOptions =
        if settings.PreRelease.IsSet then
            if settings.BumpMajor || settings.BumpMinor || settings.BumpPatch then
                Error "Cannot use --pre-release with --major, --minor, or --patch."
            else
                Ok ()
        else
            Ok ()

    result {
        do! bumpOptions
        do! preReleaseOptions
    }
