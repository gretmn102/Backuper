module Backuper.Utils
[<RequireQualifiedAccess>]
module FileAttributes =
    let setNotReadOnly att =
        if System.IO.FileAttributes.ReadOnly = (att &&& System.IO.FileAttributes.ReadOnly) then
            att ^^^ System.IO.FileAttributes.ReadOnly
        else
            att

[<RequireQualifiedAccess>]
module Path =
    let combine =
        // TODO: use `System.IO.Path.Combine`
        sprintf "%s\\%s"

module Map =
    // todo: use FsharpMyExtension.Map.mapFold
    let mapFold f (st: 'State) m =
        m
        |> Map.fold (fun (acc, st) (k: 'Key) (v: 'Value) ->
                let (x, st) = f st k v
                Map.add k x acc, st)
            (Map.empty, st)
