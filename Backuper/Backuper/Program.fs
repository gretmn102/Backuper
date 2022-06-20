module Cataloguer
open FsharpMyExtension

type T = (string * string) list

[<EntryPoint>]
let main argv =
    match argv with
    | [||] ->
        let path = "input.json"
        if System.IO.File.Exists path then
            try
                let xs: T = Json.desf path
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
        let xs: T = [srcDir, dstDir]
        xs |> List.iter Backuper.startWithMould
        0

    | _ ->
        printfn "empty args or <srcDir> <dstDir>"
        -1
