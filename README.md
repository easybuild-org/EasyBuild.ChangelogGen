# EasyBuild.ChangelogGen

[![NuGet](https://img.shields.io/nuget/v/EasyBuild.ChangelogGen.svg)](https://www.nuget.org/packages/EasyBuild.ChangelogGen)

[![Sponsors badge link](https://img.shields.io/badge/Sponsors_this_project-EA4AAA?style=for-the-badge)](https://mangelmaxime.github.io/sponsors/)

Tool for generating changelog based on Git history based on [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/). It is using [EasyBuild.CommitParser](https://github.com/easybuild-org/EasyBuild.CommitParser) to parse commit messages check their documentation for more information about configuration.

## Usage

```bash
# Install the tool
dotnet tool install EasyBuild.ChangelogGen

# Run the tool
dotnet changelog-gen
```

### CLI manual

```txt
USAGE:
    changelog-gen [changelog] [OPTIONS] [COMMAND]

ARGUMENTS:
    [changelog]    Path to the changelog file. Default is CHANGELOG.md

OPTIONS:
                                     DEFAULT
    -h, --help                                  Prints help information
    -v, --version                               Prints version information
    -c, --config                                Path to the configuration file
        --allow-dirty                           Allow to run in a dirty repository
                                                (having not commit changes in your
                                                reporitory)
        --allow-branch <VALUES>                 List of branches that are allowed to
                                                be used to generate the changelog.
                                                Default is 'main'
        --tag <VALUES>                          List of tags to include in the
                                                changelog
        --pre-release [PREFIX]       beta       Indicate that the generated version is
                                                a pre-release version. Optionally, you
                                                can provide a prefix for the beta
                                                version. Default is 'beta'
        --force-version <VERSION>               Force the version to be used in the
                                                changelog
        --skip-invalid-commit                   Skip invalid commits instead of
                                                failing
        --dry-run                               Run the command without writing to the
                                                changelog file, output the result in
                                                STDOUT instead
        --github-repo <REPO>                    GitHub repository name in format
                                                'owner/repo'

COMMANDS:
    version
```

### How is the version calculated?

### Stable versions

The version is calculated based on the commit messages since last released.

Rules are the following:

- A `breaking change` commit will bump the major version

    ```text
    * chore: release 1.2.10
    * feat!: first feature # => 2.0.0
    ```

- `feat` commits will bump the minor version

    ```text
    * chore: release 1.2.10
    * feat: first feature
    * feat: second feature # => 1.3.0
    ```

- `fix` commits will bump the patch version

    ```text
    * chore: release 1.2.10
    * fix: first fix
    * fix: second fix # => 1.2.11
    ```

You can mix different types of commits, the highest version will be used (`breaking change` > `feat` > `fix`).

```text
* chore: release 1.2.10
* feat: first feature
* fix: first fix # => 1.3.0
```

### Pre-release versions

Passing `--pre-release [PREFIX]` will generate a pre-release version.

Rules are the following:

- If the previous version is **stable**, then we compute the standard version bump and start a new pre-release version.

    ```text
    * chore: release 1.2.10
    * feat: first feature
    * fix: first fix # => 1.3.0-beta.1
    ```

- If the previous version is a **pre-release**, with the same suffix, then we increment the pre-release version.

    ```text
    * chore: release 1.3.0-beta.10
    * feat: first feature
    * fix: first fix # => 1.3.0-beta.11
    ```

- If the previous version is a **pre-release**, with a different suffix, then we use the same base version and start a new pre-release version.

    ```text
    * chore: release 1.3.0-alpha.10
    * feat: first feature
    * fix: first fix # => 1.3.0-beta.1
    ```

**💡 Tips**

EasyBuild.Changelog use the last version in the changelog file to compute the next version.

For this reason, while working on a pre-release, it is advised to work in a separate branch from the main branch. This allows you to work on the pre-release while still being able to release new versions on the main branch.

```text
* chore: release 1.2.10
| \
|  * feat!: remove `foo` API
|  * feat: add `bar` API        # => 2.0.0.beta.1
|  * fix: fix `baz` API
* fix: fix `qux` API
* chore: release 1.2.11
|  * fix: fix `qux` API         # => 2.0.0.beta.2
| /
* chore: release 2.0.0          # => 2.0.0
```

### Moving out of pre-release

If you want to move out of pre-release, you simply need to remove the `--pre-release` CLI options.

Then the next version will be released using the base version of the previous pre-release.

```text
* chore: release 1.3.0-beta.10
* feat: first feature
* fix: first fix # => 1.3.0
```

If you are not sure what will be calculated, you can use the `--dry-run` option to see the result without writing it to the changelog file.

### Overriding the version

If the computed version is not what you want, you can use the `--force-version` option to override the version to any value you want.

```bash
dotnet changelog-gen --force-version 2.0.0
```

## Monorepo support

EasyBuild.ChangelogGen supports monorepo. To do so, it use the `Tag` footer as specified in [EasyBuild.CommitParser](https://github.com/easybuild-org/EasyBuild.CommitParser).

For example, if we have the following 3 commits:

```text
----------------------------------------------
feat: add interface support

Tag: converter
----------------------------------------------
feat: add `export` support

Tag: converter
----------------------------------------------
feat: add new CLI options

Tag: cli
----------------------------------------------
```

Then I can run `dotnet changelog-gen src/converter/CHANGELOG.md --tag converter` to generate the changelog using only the commits with the `converter` tag.

```bash
dotnet changelog-gen src/converter/CHANGELOG.md --tag converter
```

## Exit codes

- `0`: Success
- `1`: Error
- `100`: Help was requested (allow other tools to detect if help was requested). This is left to the user to decide if they want to treat this as an error or not.
