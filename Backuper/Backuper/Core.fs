module Backuper
open FsharpMyExtension

open GitIgnore

module V1 =
    let extract (dir : string) =
        let isMatch startat input =
            // let r = System.Text.RegularExpressions.Regex("sdf")
            List.exists (fun (x:System.Text.RegularExpressions.Regex) ->
                // // x.IsMatch(input, startat) // почему не так? Если образец начинается на "^", то при любом аргументе `startat` кроме 0, выбивает false. Грустно не правда ли?
                // // зато ппи следующем раскладе все нормально.
                // // try
                // // x.Match(input, startat, input.Length - startat)
                // // |> fun x -> x.Success
                // // with e -> failwithf "%A\n%s" x input
                // // Если не это повод спиться, то я уже не знаю что тогда.
                x.IsMatch(input, startat)
            )
        let rec f ((includesf, excludesf):(string -> bool) * (string -> bool)) path =
            let files =
                System.IO.Directory.EnumerateFiles path
                |> Seq.filter (fun path ->
                    not (excludesf path) || includesf path
                )
            let dirs =
                System.IO.Directory.GetDirectories path
                |> Seq.collect (fun dir ->
                    if not (excludesf dir) || includesf dir then
                        let gitignorePath = dir + "\\.gitignore"
                        if System.IO.File.Exists gitignorePath then
                            let startat = dir.Length
                            let includes, excludes = gitignoreLoad gitignorePath
                            let excludesf input = isMatch startat input excludes
                            let includesf input = isMatch startat input includes
                            f (includesf, excludesf) dir
                        else
                            f (includesf, excludesf) dir
                    else Seq.empty
                    )
            Seq.append files dirs
        let dir = dir.TrimEnd [| '\\' |]

        let gitignorePath = dir + "\\.gitignore"
        if System.IO.File.Exists gitignorePath then
            let startat = dir.Length
            let includes, excludes = gitignoreLoad gitignorePath
            let excludesf input = isMatch startat input excludes
            let includesf input = isMatch startat input includes
            f (includesf, excludesf) dir
        else
            failwith "not found global .gitignore"

    // let test' () =
    //     let path = @"e:\Project\Sandbox\Sandbox 2\"
    //     let exp = extract path |> Array.ofSeq
    //     let act = extract2 path |> Array.ofSeq
    //     exp = act
    let extractAndCopy overwrite (srcDir:string) (dstDir:string) =
        let srcDir, dstDir = srcDir.TrimEnd [|'\\'|], dstDir.TrimEnd [|'\\'|]
        extract srcDir
        |> Seq.iter (fun src ->
            let dst = dstDir + src.[srcDir.Length..]
            let dir = System.IO.Path.GetDirectoryName dst
            if not <| System.IO.Directory.Exists dir then
                System.IO.Directory.CreateDirectory dir |> ignore
            if overwrite then
                System.IO.File.Copy(src, dst, true)
            elif not <| System.IO.File.Exists dst then
                System.IO.File.Copy(src, dst)
        )
    // extractAndCopy false @"E:\Project\Parsers\KrkrToRenpy" @"e:\Project2"

module V2 =
    // Замена по слепку.
    // Сейчас программа просто копирует файлы согласно с .gitignore, это долго и напрасно: ибо заменяет все подряд, хотя можно было бы сравнить дату записи файлов... О, а это мысль: просто сравнивать дату записи. Но как быть с удаленными элементами?
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
    let isMatch startat input =
        // let r = System.Text.RegularExpressions.Regex("sdf")
        List.exists (fun (x:System.Text.RegularExpressions.Regex) ->
            // // x.IsMatch(input, startat) // почему не так? Если образец начинается на "^", то при любом аргументе `startat` кроме 0, выбивает false. Грустно не правда ли?
            // // зато ппи следующем раскладе все нормально.
            // // try
            // // x.Match(input, startat, input.Length - startat)
            // // |> fun x -> x.Success
            // // with e -> failwithf "%A\n%s" x input
            // // Если не это повод спиться, то я уже не знаю что тогда.
            x.IsMatch(input, startat)
        )
    /// всегда учитывает папку ".git"
    let extract2 (includesf, excludesf) (dir:System.IO.DirectoryInfo) =
        let rec enumerateWithoutRules (dir:System.IO.DirectoryInfo) =
            let files =
                dir.EnumerateFiles()
                |> Seq.fold (fun st x -> Map.add x.Name x.LastWriteTime st ) Map.empty
            let dirs =
                dir.EnumerateDirectories()
                |> Seq.fold (fun st dir -> Map.add dir.Name (enumerateWithoutRules dir) st) Map.empty
            T(dirs, files)

        let rec f ((includesf, excludesf):(string -> bool) * (string -> bool)) (dir:System.IO.DirectoryInfo) =
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
                            // seq{
                            //     yield! System.IO.Directory.GetFiles path
                            //     yield! System.IO.Directory.GetDirectories path
                            //            |> Array.map f |> Seq.concat
                            // }
                        let x = enumerateWithoutRules dir
                        Map.add dir.Name x st
                    else
                        let path = dir.FullName
                        if not (excludesf path) || includesf path then
                            let x =
                                let gitignorePath = path + "\\.gitignore"
                                // printfn "%s" gitignorePath
                                if System.IO.File.Exists gitignorePath then
                                    // printfn "true"
                                    // let dir = dir.Name
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
    let extract (dir : string) =
        let dirPath = dir.TrimEnd [| '\\' |]
        let dir = System.IO.DirectoryInfo dirPath
        let gitignorePath = dirPath + "\\.gitignore"
        let includes, excludes =
            if System.IO.File.Exists gitignorePath then
                gitignoreLoad gitignorePath
            else
                [], []
                // failwith "not found global .gitignore"
        let startat = dirPath.Length
        let excludesf input = isMatch startat input excludes
        let includesf input = isMatch startat input includes
        extract2 (includesf, excludesf) dir
    // let extractWithoutGitIgnore (dir : string) =
    //     let dirPath = dir.TrimEnd [| '\\' |]
    //     let dir = System.IO.DirectoryInfo dirPath
    //     let startat = dirPath.Length
    //     let includes, excludes = [], []
    //     let excludesf input = isMatch startat input excludes
    //     let includesf input = isMatch startat input includes
    //     extract2 (includesf, excludesf) dir

    // let extractMany (dirs : string list) =
    //     // let m =
    //     //     dirs |> Seq.fold (fun st x ->
    //     //                 Map.add x (extractWithoutGitIgnore x) st ) Map.empty
    //     // T(m, Map.empty)
    //     dirs |> List.map extract
    type Ty =
        | NotChanged
        | Added
        | Changed
        /// элемент, отсеянный .gitignore
        | Removed
    type Diff = Diff of Map<DirectoryName,Diff> * Map<FileName, System.DateTime * Ty>
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
        let mapFold f (st:'State) m =
            m
            |> Map.fold (fun (acc, st) (k:'Key) (v:'Value) ->
                    let (x, st) = f st k v
                    Map.add k x acc, st)
                (Map.empty, st)

    /// новый слепок -> старый слепок
    let rec diff (T(dsNew, fsNew)) (T(dsOld,fsOld)) : Diff =
        let fs =
            // let fsOldRest, fileNameFulls =
            //     fsNew
            //     |> Map.fold (fun (fsOld, acc) fileName fileWriteNew ->
            //         match Map.tryFind fileName fsOld with
            //         | Some fileWriteOld ->
            //             let acc =
            //                 if fileWriteOld = fileWriteNew then
            //                     acc
            //                 else
            //                     toFileNameFull fileName::acc
            //             Map.remove fileName fsOld, acc
            //         | None -> fsOld, toFileNameFull fileName::acc
            //     ) (fsOld, [])
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
                                // toFileNameFull fileName::acc
                        acc, Map.remove fileName fsOld
                    | None -> (fileWriteNew, Added), fsOld
                    ) fsOld
            // из старого слепка остались файлы: либо удаленные, либо те, что .gitignore отсеял, но их все равно нужно сохранить. Так по крайней делает Git.
            // fsOldRest
            // |> Map.fold (fun (fsNew, acc) name lastWrite ->
            //     let fileNameFull = toFileNameFull name
            //     if System.IO.File.Exists (sprintf "%s\\%s" srcDir fileNameFull) then
            //         Map.add name lastWrite fsNew, fileNameFull::acc
            //     else
            //         delete (sprintf "%s\\%s" dstDir fileNameFull)
            //         fsNew, acc
            // ) (fsNew, fileNameFulls)
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
        // I have some doubts about this, but whatever.
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
let test2 () =
    let dirPath = @"e:\Project"

    let exp = V1.extract dirPath |> Set.ofSeq
    let act = V2.extract dirPath |> V2.TT.unpack dirPath |> Set.ofSeq
    exp = act
    Set.difference act exp
    Set.contains @"e:\Project\Server\Server\Server\App.config" exp

    let dirPath = @"e:\Project\temp"
    let exp' = V1.extract dirPath |> Set.ofSeq
    let act' = V2.extract dirPath |> V2.TT.unpack dirPath |> Set.ofSeq
    Set.difference act' exp'
    exp' = act'

    let dir = System.IO.DirectoryInfo dirPath
    // dir.GetDirectories "*" |> Array.head
    // |>
    ()
open V2

let start srcDir dstDir =
    // let srcDir = @"e:\All2\Projects"
    // let dstDir = @"h:\ProjectsBackup"
    System.IO.Directory.CreateDirectory dstDir |> ignore
    let mouldPath = dstDir + "\\mould.json"
    printfn "build mould from %s..." srcDir
    let mouldNew = V2.extract srcDir
    printfn "built"
    let mouldOld = V2.T(Map.empty, Map.empty)
    let diff = V2.diff mouldNew mouldOld
    let diff = V2.apply true srcDir dstDir diff
    let mould = Diff.toT diff
    Json.serfNotIdent mouldPath mould
// start @"e:\Disc D\All" @"h:\AllBackup"
// let startMany () =
//     let srcDir = @"e:\Project"
//     let dstDir = @"g:\Notes\New"
//     let mouldPath = dstDir + "\\mould.json"
//     let mould = V2.extractMany [ @"d:\Chief\Cataloguer"; @"d:\Chief\ServerSamopal"]
//     let mouldNew = V2.T(Map.empty, Map.empty)
//     let diff = V2.diff mould mouldNew
//     let diff = V2.apply srcDir dstDir diff
//     printfn "применил"
//     let mould = Diff.toT diff
//     Json.serfNotIdent mouldPath mould
// start2()
let startWithMould paths =
    // let srcDir = @"e:\Project"
    // let dstDir = @"g:\Notes\ProjBackup"
    let srcDir, dstDir = mapBoth System.IO.Path.GetFullPath paths
    let mouldPath = dstDir + "\\mould.json"
    if System.IO.File.Exists mouldPath then
        let mouldOld : T = Json.desf mouldPath
        printfn "build mould from %s..." srcDir
        let mouldNew = extract srcDir
        printfn "built"
        let diff = diff mouldNew mouldOld
        // diff |> Json.serf "output\\output.json"
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

let test3 () =
    // let srcDir = @"e:\Project"
    // let dstDir = @"h:\FromFlashCard\Backup"
    // let old = V2.extract dstDir
    // Json.serfNotIdent mould old

    // let x = @"Cataloguer\.gitignore"
    // let src = sprintf "%s\\%s" srcDir x
    // let dst = sprintf "%s\\%s" dstDir x
    // let srcf = System.IO.FileInfo src
    // let dstf = System.IO.FileInfo dst
    // dstf.Attributes <- dstf.Attributes ^^^ System.IO.FileAttributes.ReadOnly
    // let withReadOnly = dstf.Attributes
    // let withoutReadOnly = dstf.Attributes

    // withReadOnly &&& System.IO.FileAttributes.ReadOnly
    // withoutReadOnly &&& System.IO.FileAttributes.ReadOnly
    // withoutReadOnly withReadOnly = withoutReadOnly
    // withoutReadOnly withoutReadOnly = withoutReadOnly

    // dstf.Attributes ^^^ System.IO.FileAttributes.ReadOnly
    // dstf.Attributes
    // System.IO.File.Exists dst
    // System.IO.File.Copy(src, dst, true)
    // System.IO.File.GetAttributes dst
    // let file = System.IO.FileInfo dst

    // file.Attributes <- file.Attributes ^^^ System.IO.FileAttributes.ReadOnly
    // file.Delete()
    // System.IO.File.Delete dst

    // dstf.Attributes <- System.IO.FileAttributes.ReadOnly ^^^ dstf.Attributes
    // srcf.CopyTo(dst, true)

    // // let x = System.IO.File.GetAccessControl dst
    // // x.SetAccessRule
    // System.IO.File.Copy(src, dst, true)
    ()