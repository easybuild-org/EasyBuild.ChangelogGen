module EasyBuild.ChangelogGen.Commands.Version

open Spectre.Console.Cli
open System.Reflection

type VersionSettings() =
    inherit CommandSettings()

type VersionCommand() =
    inherit Command<VersionSettings>()
    interface ICommandLimiter<VersionSettings>

    override __.Execute(context, settings) =
        let assembly = Assembly.GetEntryAssembly()
        let versionAttribute = assembly.GetCustomAttribute<AssemblyInformationalVersionAttribute>()
        let version =
            if versionAttribute <> null then
                versionAttribute.InformationalVersion
            else
                "?"

        Log.info($"Version: {version}")
        0
