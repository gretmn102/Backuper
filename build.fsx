// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "./packages/FAKE/tools/FakeLib.dll"
open System
open Fake.IO.Globbing.Operators
open Fake.Core
// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------
let testPath = !! "test/**/test.fsproj" |> Seq.tryHead
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet
let buildConf = DotNet.BuildConfiguration.Debug
let dotnetSdk = lazy DotNet.install DotNet.Versions.Release_2_1_402
let inline dtntSmpl arg = DotNet.Options.lift dotnetSdk.Value arg

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
Target.create "BuildTest" (fun _ ->
    testPath
    |> Option.defaultWith (fun () -> failwith "'**/test.fsproj' not found")
    |> System.IO.Path.GetDirectoryName
    |> DotNet.build (fun x ->
        { x with Configuration = buildConf }
        |> dtntSmpl)
    // appReferences
    // |> Seq.iter (fun p ->
    //     let dir = System.IO.Path.GetDirectoryName p
    //     DotNet.build (dtntWorkDir root) dir
    // )
)

Target.create "Test" (fun _ ->
    testPath
    |> Option.bind (fun p ->
        let dir = System.IO.Path.GetDirectoryName p
        let d = sprintf @"bin\%A\net461\test.exe" buildConf
        let dir = System.IO.Path.Combine(dir, d)
        if System.IO.File.Exists dir then Some dir else None
    )
    |> Option.map (fun dir ->
        let result =
            Process.execSimple (fun info ->
                info.WithFileName dir) TimeSpan.MaxValue
        if result <> 0 then failwith "tests failed"
    )
    |> Option.defaultWith (fun () -> failwith "test not found" )
)

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators
// "Clean"
//   ==> "Restore"
//   ==> "Build"
//   ==> "Test"


"BuildTest"
  ==> "Test"
Target.runOrDefault "Test"