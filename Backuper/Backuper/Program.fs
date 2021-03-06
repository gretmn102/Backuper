module Cataloguer
#if INTERACTIVE
#load "Backuper.fs"
#endif
open FsharpMyExtension
let test () =
    let xs = Backuper.V2.extract @"E:\Sandbox\User\DefaultBox\drive\E\renpy"
    xs |> FsharpMyExtension.Json.serf "output\\output.json"
    let oldMouldPath = @"h:\RenpyBackup\mould.json"
    let oldMould : Backuper.V2.T = Json.desf oldMouldPath
    Backuper.V2.diff xs oldMould
    |> Json.serf "output\\diff.json"

// Итак. Что здесь видеть?
// Было бы неплохо разметить элементы диска на теги.
// Чем плохи обычные папки?

type T = (string * string) list

[<EntryPoint>]
let main argv =
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