module Cataloguer
open FsharpMyExtension

open Backuper.Core

let start srcDir dstDir =
    System.IO.Directory.CreateDirectory dstDir |> ignore
    // todo: use `System.IO.Path.Combine`
    let mouldPath = dstDir + "\\mould.json"
    printfn "build mould from %s..." srcDir
    let mouldNew = DirTree.build srcDir
    printfn "built"
    let mouldOld = DirTree(Map.empty, Map.empty)
    let diff = DirDiffTree.diff mouldNew mouldOld
    let diff = DirDiffTree.apply true srcDir dstDir diff
    let mould = DirDiffTree.toDirTree diff
    Json.serfNotIdent mouldPath mould

let startWithMould paths =
    let srcDir, dstDir = mapBoth System.IO.Path.GetFullPath paths
    // todo: use `System.IO.Path.Combine`
    let mouldPath = dstDir + "\\mould.json"
    if System.IO.File.Exists mouldPath then
        let mouldOld: DirTree = Json.desf mouldPath
        printfn "build mould from %s..." srcDir
        let mouldNew = DirTree.build srcDir
        printfn "built"
        let diff = DirDiffTree.diff mouldNew mouldOld
        printfn "applying difference moulds..."
        let diff = DirDiffTree.apply true srcDir dstDir diff
        printfn "applied"
        let mould = DirDiffTree.toDirTree diff
        Json.serfNotIdent mouldPath mould
    else
        printfn "'%s' not exists. Create? (y/n)" mouldPath
        let rec f () =
            let k = System.Console.ReadKey()
            match k.Key with
            | System.ConsoleKey.Y ->
                start srcDir dstDir
            | System.ConsoleKey.N ->
                printfn @"¯\_(ツ)_/¯"
            | x ->
                printf "expected 'y' or 'n' but '%A'. Try again." x
                f()
        f()

type Setting = (string * string) list

[<EntryPoint>]
let main argv =
    match argv with
    | [||] ->
        let path = "input.json"
        if System.IO.File.Exists path then
            try
                let xs: Setting = Json.desf path
                xs |> List.iter startWithMould
                0
            with e ->
                printfn "%A" e
                System.Console.ReadKey() |> ignore
                -1
        else
            printfn "not found %A" path
            System.Console.ReadKey() |> ignore
            -1

    | [|srcDir; dstDir|] ->
        let xs: Setting = [srcDir, dstDir]
        xs |> List.iter startWithMould
        0

    | _ ->
        printfn "empty args or <srcDir> <dstDir>"
        -1
