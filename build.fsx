// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref Build //"
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard"

open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------
let f projName =
    let pattern = sprintf @"**/%s.fsproj" projName
    let xs = !! pattern
    xs
    |> Seq.tryExactlyOne
    |> Option.defaultWith (fun () ->
        xs
        |> List.ofSeq
        |> failwithf "'%s' expected exactly one but:\n%A" pattern
    )

let testProjName = "Test"
let testProjPath = sprintf "Test/Test/%s.fsproj" testProjName
let testProjDir = Path.getDirectory testProjPath
let cliProjName = "backuper-cli"
let cliProjPath = f cliProjName
let cliProjDir = Path.getDirectory cliProjPath
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir
// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
let commonBuildArgs = "-c Release"

Target.create "RunTests" (fun _ ->
    testProjDir
    |> dotnet (sprintf "run %s" commonBuildArgs)
)

Target.create "BuildCli" (fun _ ->
    cliProjDir
    |> dotnet (sprintf "build %s" commonBuildArgs)
)
// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

Target.runOrDefault "BuildCli"
