module EasyBuild.ChangelogGen.Types

open Thoth.Json.Core

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
