module Program
// type T2 = File | Dir
type File = string
type T =
    //| Dir of Map<string, T array * File array>
    //| File of string
    //| Node of T2 * string
    //| Dir2 of string * (T array * File array)
    | Dir of Map<string,T> * File array


let rec extract path =
    let files = System.IO.Directory.GetFiles path
    let dirs = System.IO.Directory.GetDirectories path
    let sub =
        dirs
        |> Array.fold (fun st x ->
            Map.add (System.IO.Path.GetFileName x) (extract x) st) Map.empty
    Dir(sub, Array.map System.IO.Path.GetFileName files)
// open FsharpMyExtension
// let xs = extract @"e:\Project"

// xs
// |> Json.serfNotIdent "output\\output.json"


// extract @"c:\Users\Default\Application Data"
// System.IO.Directory.EnumerateFiles @"c:\Users\Default\Application Data"
// System.IO.Directory.EnumerateFiles @"c:\Users\Все пользователи"