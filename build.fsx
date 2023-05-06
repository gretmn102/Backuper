// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref Build //"
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------
let testProjName = "Test"
let testProjDir = "Test/Test"
let testProjPath = sprintf "%s/%s.fsproj" testProjDir testProjName
let mainProjName = "backuper-cli"
let mainProjPath = sprintf "src/backuper-cli/%s.fsproj" mainProjName
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet
let buildConf = DotNet.BuildConfiguration.Release

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
Target.create "BuildTest" (fun _ ->
    // Unsupported log file format. Latest supported version is 9, the log file has version 14.
    // testProjPath
    // |> Fake.IO.Path.getDirectory
    // |> DotNet.build (fun x ->
    //     { x with Configuration = buildConf }
    // )
    let args = sprintf "--configuration Release"
    let result = Fake.DotNet.DotNet.exec (Fake.DotNet.DotNet.Options.withWorkingDirectory testProjDir) "build" args
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" "build" testProjPath
)

let run projName projPath =
    let dir = Fake.IO.Path.getDirectory projPath
    let localpath = sprintf "bin/%A/net461/%s.exe" buildConf projName
    let path = Fake.IO.Path.combine dir localpath
    if not <| Fake.IO.File.exists path then
        failwithf "not found %s" path

    Command.RawCommand(path, Arguments.Empty)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory (Fake.IO.Path.getDirectory path)
    |> Proc.run

Target.create "RunTest" (fun _ ->
    let x = run testProjName testProjPath
    if x.ExitCode <> 0 then
        failwith "test error"
)

Target.create "RunMainProj" (fun _ ->
    run mainProjName mainProjPath |> ignore
)
// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

"BuildTest"
  ==> "RunTest"
  ==> "RunMainProj"
Target.runOrDefault "RunTest"
