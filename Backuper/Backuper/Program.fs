module Cataloguer
#if INTERACTIVE
#load "Core.fs"
#endif
open FsharpMyExtension
let test () =
    System.Environment.CurrentDirectory <- @"e:\Project\Template" // __SOURCE_DIRECTORY__
    let srcDir = @"Src"
    let srcDir = System.IO.Path.GetFullPath srcDir
    let xs = Backuper.extract srcDir
    xs |> FsharpMyExtension.Json.serf "output\\output.json"
    let oldMouldPath = @"h:\RenpyBackup\mould.json"
    let oldMould : Backuper.T = Json.desf oldMouldPath
    Backuper.diff xs oldMould
    |> Json.serf "output\\diff.json"

// Итак. Что здесь видеть?
// Было бы неплохо разметить элементы диска на теги.
// Чем плохи обычные папки?

type T = (string * string) list

[<EntryPoint>]
let main argv =
    match argv with
    | [||] ->
        let path = "input.json"
        if System.IO.File.Exists path then
            try
                let xs:T = Json.desf path
                xs |> List.iter Backuper.startWithMould
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
        let xs:T = [srcDir, dstDir]
        xs |> List.iter Backuper.startWithMould
        0
    | _ ->
        printfn "empty args or <srcDir> <dstDir>"
        -1