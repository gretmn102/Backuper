﻿module Program
// type T2 = File | Dir
type File = string
type T =
    | Dir of Map<string,T> * File array


let rec extract path =
    let files = System.IO.Directory.GetFiles path
    let dirs = System.IO.Directory.GetDirectories path
    let sub =
        dirs
        |> Array.fold (fun st x ->
            Map.add (System.IO.Path.GetFileName x) (extract x) st) Map.empty
    Dir(sub, Array.map System.IO.Path.GetFileName files)
