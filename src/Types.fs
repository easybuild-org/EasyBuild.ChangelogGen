module EasyBuild.ChangelogGen.Types

type RemoteConfig =
    {
        Hostname: string
        Owner: string
        Repository: string
    }

    /// <summary>
    /// Returns the baseUrl for the remote in the formet of <c>https://hostname/owner/repository</c>
    /// </summary>
    /// <returns>Base URL</returns>
    member this.BaseUrl = $"https://%s{this.Hostname}/%s{this.Owner}/%s{this.Repository}"

    /// <summary>
    /// Returns the name of the remote based on the hostname.
    ///
    /// E.g. for <c>github.com</c> it returns <c>github</c>
    /// </summary>
    /// <returns></returns>
    member this.NameOnly =
        this.Hostname
        |> Seq.toList
        |> List.takeWhile (fun c -> c <> '.')
        |> List.map string
        |> String.concat ""
