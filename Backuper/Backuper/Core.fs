module Backuper
open FsharpMyExtension

open GitIgnore

type FileName = string
type DirectoryName = string
type T = T of Map<DirectoryName,T> * Map<FileName, System.DateTime>

module TT =
    let rec map f (T(ds,fs)) =
        let fs = fs |> Map.map (fun k v -> f k v )
        let ds = ds |> Map.map (fun _ -> map f)
        T(ds, fs)

    let rec fold name f st (T(ds,fs)) =
        let fs =
            fs |> Map.fold (fun st k v ->
                f st name k v ) st
        ds |> Map.fold (fun st k v ->
            fold k f st v) fs

    let rec unpack path (T(ds,fs)) =
        seq {
            yield! Map.fold (fun st name dateTime ->
                sprintf "%s\\%s" path name |> List.consFlip st) [] fs
            yield! Map.fold (fun st name x ->
                let path = sprintf "%s\\%s" path name
                unpack path x
                |> List.consFlip st) [] ds
                |> Seq.concat
        }

let isMatch startAt input =
    List.exists (fun (x: System.Text.RegularExpressions.Regex) ->
        x.IsMatch(input, startAt)
    )

/// всегда учитывает папку ".git"
let extract2 (includesf, excludesf) (dir: System.IO.DirectoryInfo) =
    let rec enumerateWithoutRules (dir: System.IO.DirectoryInfo) =
        let files =
            dir.EnumerateFiles()
            |> Seq.fold (fun st x -> Map.add x.Name x.LastWriteTime st ) Map.empty
        let dirs =
            dir.EnumerateDirectories()
            |> Seq.fold (fun st dir -> Map.add dir.Name (enumerateWithoutRules dir) st) Map.empty
        T(dirs, files)

    let rec f ((includesf, excludesf): (string -> bool) * (string -> bool)) (dir: System.IO.DirectoryInfo) =
        let files =
            dir.GetFiles()
            |> Array.fold (fun st x ->
                let path = x.FullName
                if not (excludesf path) || includesf path then
                    Map.add x.Name x.LastWriteTime st
                else
                    st
                ) Map.empty

        let dirs =
            dir.GetDirectories()
            |> Array.fold (fun st dir ->
                if dir.Name = ".git" then
                    let x = enumerateWithoutRules dir
                    Map.add dir.Name x st
                else
                    let path = dir.FullName
                    if not (excludesf path) || includesf path then
                        let x =
                            let gitignorePath = path + "\\.gitignore"

                            if System.IO.File.Exists gitignorePath then
                                let startat = dir.FullName.Length
                                let includes, excludes = gitignoreLoad gitignorePath
                                let excludesf input = isMatch startat input excludes
                                let includesf input = isMatch startat input includes
                                f (includesf, excludesf) dir
                            else
                                f (includesf, excludesf) dir

                        let (T(ds, fs)) = x

                        if Map.isEmpty ds && Map.isEmpty fs then st
                        else
                            Map.add dir.Name x st
                    else
                        st
            ) Map.empty

        T(dirs, files)

    f (includesf, excludesf) dir

let extract (dir: string) =
    let dirPath = dir.TrimEnd [| '\\' |]
    let dir = System.IO.DirectoryInfo dirPath
    let gitignorePath = dirPath + "\\.gitignore"
    let includes, excludes =
        if System.IO.File.Exists gitignorePath then
            gitignoreLoad gitignorePath
        else
            [], []
    let startAt = dirPath.Length
    let excludesf input = isMatch startAt input excludes
    let includesf input = isMatch startAt input includes
    extract2 (includesf, excludesf) dir

type Ty =
    | NotChanged
    | Added
    | Changed
    /// элемент, отсеянный .gitignore
    | Removed

type Diff = Diff of Map<DirectoryName, Diff> * Map<FileName, System.DateTime * Ty>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Diff =
    let rec ofT f (T(ds,fs)) =
        let fs =
            fs |> Map.fold (fun st k v ->
                Map.add k (f k v) st ) Map.empty
        let ds =
            ds |> Map.fold (fun st k v ->
                st |> Map.add k (ofT f v)
                ) Map.empty
        Diff(ds, fs)

    let rec toT (Diff(ds,fs)) =
        let fs =
            fs |> Map.fold (fun st k v ->
                Map.add k (fst v) st ) Map.empty
        let ds =
            ds |> Map.fold (fun st k v ->
                st |> Map.add k (toT v)
                ) Map.empty
        T(ds, fs)

module Map =
    let mapFold f (st: 'State) m =
        m
        |> Map.fold (fun (acc, st) (k: 'Key) (v: 'Value) ->
                let (x, st) = f st k v
                Map.add k x acc, st)
            (Map.empty, st)

/// новый слепок -> старый слепок
let rec diff (T(dsNew, fsNew)) (T(dsOld, fsOld)): Diff =
    let fs =
        let fs, fsOldRest =
            fsNew
            |> Map.mapFold (fun fsOld fileName fileWriteNew ->
                match Map.tryFind fileName fsOld with
                | Some fileWriteOld ->
                    let acc =
                        if fileWriteOld = fileWriteNew then
                            fileWriteNew, NotChanged
                        else
                            fileWriteNew, Changed

                    acc, Map.remove fileName fsOld
                | None -> (fileWriteNew, Added), fsOld
                ) fsOld

        fsOldRest
        |> Map.fold (fun fs fileName fileWrite ->
            Map.add fileName (fileWrite, Removed) fs
        ) fs

    let ds, dsRest =
        dsNew
        |> Map.mapFold (fun dsOld dirName subNew ->
            match Map.tryFind dirName dsOld with
            | Some subOld ->
                let res = diff subNew subOld
                let stOld = Map.remove dirName dsOld
                res, stOld
            | None ->
                let res = subNew |> Diff.ofT (fun _ v -> v, Added)
                res, dsOld
        ) dsOld

    let ds =
        dsRest
        |> Map.fold (fun ds dirName subOld ->
            let res = subOld |> Diff.ofT (fun _ v -> v, Removed)
            Map.add dirName res ds
        ) ds

    Diff(ds, fs)

let setAttributeNotReadOnly att =
    if System.IO.FileAttributes.ReadOnly = (att &&& System.IO.FileAttributes.ReadOnly) then
        att ^^^ System.IO.FileAttributes.ReadOnly
    else att

let rec apply deleteNonexistent srcDir dstDir (Diff(ds, fs)) =
    let combine = sprintf "%s\\%s"
    let fs =
        fs |> Map.choose (fun fileName ((lastWriteOld, ty) as v) ->
            match ty with
            | NotChanged -> Some v
            | Added
            | Changed ->
                try
                    // TODO: make sure that the folder is created only once, and not for every changed file, as it is now
                    System.IO.Directory.CreateDirectory dstDir |> ignore
                with
                    e -> printfn "%A\n%A" (srcDir, dstDir) e.Message

                let src = combine srcDir fileName
                let dst = combine dstDir fileName

                try
                    let dstf = System.IO.FileInfo dst
                    if dstf.Exists then
                        dstf.Attributes <- setAttributeNotReadOnly dstf.Attributes

                    try
                        System.IO.File.Copy(src, dst, true)
                    with e ->
                        printfn "%A\n%A" (src, dst) e.Message
                with e ->
                    printfn "%A\n%A" (src, dst) e.Message

                Some v
            | Removed ->
                try
                    System.IO.Directory.CreateDirectory dstDir |> ignore
                with
                    e -> printfn "%A\n%A" (srcDir, dstDir) e.Message

                let src = combine srcDir fileName
                let dst = combine dstDir fileName
                try
                    let dstf = System.IO.FileInfo dst
                    if dstf.Exists then
                        dstf.Attributes <- setAttributeNotReadOnly dstf.Attributes
                    // // так работает нормальный .git:
                    // let srcf = new System.IO.FileInfo(src)
                    // if srcf.Exists then
                    //     let lastWriteNew = srcf.LastWriteTime
                    //     if lastWriteNew = lastWriteOld then
                    //         Some v
                    //     else
                    //         srcf.CopyTo(dst, true) |> ignore
                    //         Some(lastWriteNew, ty)
                    // else
                    //     if deleteNonexistent then
                    //         if dstf.Exists then dstf.Delete()
                    //         None
                    //     else
                    //         Some v
                    // // но нам, стало быть, такое не надо.
                    if deleteNonexistent then
                        if dstf.Exists then dstf.Delete()
                        None
                    else
                        Some v
                with e ->
                    printfn "%A\n%A" (src, dst) e.Message
                    Some v
        )
    let ds =
        ds |> Map.map (fun dirName ->
            let src = combine srcDir dirName
            let dst = combine dstDir dirName

            apply deleteNonexistent src dst
        )
    Diff(ds, fs)

let start srcDir dstDir =
    System.IO.Directory.CreateDirectory dstDir |> ignore
    let mouldPath = dstDir + "\\mould.json"
    printfn "build mould from %s..." srcDir
    let mouldNew = extract srcDir
    printfn "built"
    let mouldOld = T(Map.empty, Map.empty)
    let diff = diff mouldNew mouldOld
    let diff = apply true srcDir dstDir diff
    let mould = Diff.toT diff
    Json.serfNotIdent mouldPath mould

let startWithMould paths =
    let srcDir, dstDir = mapBoth System.IO.Path.GetFullPath paths
    let mouldPath = dstDir + "\\mould.json"
    if System.IO.File.Exists mouldPath then
        let mouldOld: T = Json.desf mouldPath
        printfn "build mould from %s..." srcDir
        let mouldNew = extract srcDir
        printfn "built"
        let diff = diff mouldNew mouldOld
        printfn "applying difference moulds..."
        let diff = apply true srcDir dstDir diff
        printfn "applied"
        let mould = Diff.toT diff
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
