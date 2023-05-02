module Backuper
open FsharpMyExtension

type FileName = string
type DirName = string
type LastWriteTime = System.DateTime
type DirTree = DirTree of Map<DirName,DirTree> * Map<FileName, LastWriteTime>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module DirTree =
    let isEmpty (DirTree(dirs, files)) =
        Map.isEmpty dirs && Map.isEmpty files

    let rec buildWithoutGitIgnore (dirInfo: System.IO.DirectoryInfo) =
        let files =
            dirInfo.EnumerateFiles()
            |> Seq.fold
                (fun st fileInfo ->
                    Map.add fileInfo.Name fileInfo.LastWriteTime st
                )
                Map.empty

        let dirs =
            dirInfo.EnumerateDirectories()
            |> Seq.fold
                (fun st dir ->
                    Map.add dir.Name (buildWithoutGitIgnore dir) st
                )
                Map.empty

        DirTree(dirs, files)

    let rec buildWithGitIgnore (gitIgnore: Git.GitIgnore) (dirInfo: System.IO.DirectoryInfo) =
        let files =
            dirInfo.GetFiles()
            |> Array.fold
                (fun st x ->
                    let path = x.FullName
                    if Git.GitIgnore.isMatch path gitIgnore then
                        Map.add x.Name x.LastWriteTime st
                    else
                        st
                )
                Map.empty

        let dirs =
            dirInfo.GetDirectories()
            |> Array.fold
                (fun st dir ->
                    if dir.Name = ".git" then
                        let x = buildWithoutGitIgnore dir
                        Map.add dir.Name x st
                    else
                        let path = dir.FullName
                        if Git.GitIgnore.isMatch path gitIgnore then
                            let dirTree =
                                let gitIgnorePath = path + "\\.gitignore"

                                if System.IO.File.Exists gitIgnorePath then
                                    let startAt = dir.FullName.Length
                                    let gitIgnore =
                                        Git.GitIgnore.create startAt gitIgnorePath
                                    buildWithGitIgnore gitIgnore dir
                                else
                                    buildWithGitIgnore gitIgnore dir

                            if isEmpty dirTree then
                                st
                            else
                                Map.add dir.Name dirTree st
                        else
                            st
                )
                Map.empty

        DirTree(dirs, files)

    let extract (dir: string) =
        let dirPath = dir.TrimEnd [| '\\' |]
        let dir = System.IO.DirectoryInfo dirPath
        let gitIgnorePath = dirPath + "\\.gitignore"
        let gitIgnore =
            if System.IO.File.Exists gitIgnorePath then
                let startAt = dirPath.Length
                Git.GitIgnore.create startAt gitIgnorePath
            else
                Git.GitIgnore.empty
        buildWithGitIgnore gitIgnore dir

module Utils =
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

[<Struct>]
type DiffType =
    | NotChanged
    | Changed
    | Added
    | Removed

type DirDiffTree = DirDiffTree of Map<DirName, DirDiffTree> * Map<FileName, System.DateTime * DiffType>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module DirDiffTree =
    open Utils

    let rec ofDirTree calcDiff (DirTree(dirs, files)) =
        let files =
            files
            |> Map.fold
                (fun st fileName dateTime ->
                    st |> Map.add fileName (calcDiff fileName dateTime)
                )
                Map.empty

        let dirs =
            dirs
            |> Map.fold
                (fun st dirName dirTree ->
                    st |> Map.add dirName (ofDirTree calcDiff dirTree)
                )
                Map.empty

        DirDiffTree(dirs, files)

    let rec toDirTree (DirDiffTree(dirs, files)) =
        let files =
            files
            |> Map.fold
                (fun st fileName (dateTime, _) ->
                    st |> Map.add fileName dateTime
                )
                Map.empty

        let dirs =
            dirs
            |> Map.fold
                (fun st dirName dirDiffTree ->
                    st |> Map.add dirName (toDirTree dirDiffTree)
                )
                Map.empty

        DirTree(dirs, files)

    let rec diff (DirTree(dsNew, fsNew)) (DirTree(dsOld, fsOld)): DirDiffTree =
        let fs =
            let fs, fsOldRest =
                fsNew
                |> Map.mapFold
                    (fun fsOld fileName fileWriteNew ->
                        match Map.tryFind fileName fsOld with
                        | Some fileWriteOld ->
                            let acc =
                                if fileWriteOld = fileWriteNew then
                                    fileWriteNew, NotChanged
                                else
                                    fileWriteNew, Changed

                            acc, Map.remove fileName fsOld
                        | None ->
                            (fileWriteNew, Added), fsOld
                    )
                    fsOld

            fsOldRest
            |> Map.fold
                (fun fs fileName fileWrite ->
                    Map.add fileName (fileWrite, Removed) fs
                )
                fs

        let ds, dsRest =
            dsNew
            |> Map.mapFold
                (fun dsOld dirName subNew ->
                    match Map.tryFind dirName dsOld with
                    | Some subOld ->
                        let res = diff subNew subOld
                        let stOld = Map.remove dirName dsOld
                        res, stOld
                    | None ->
                        let res = subNew |> ofDirTree (fun _ v -> v, Added)
                        res, dsOld
                )
                dsOld

        let ds =
            dsRest
            |> Map.fold
                (fun ds dirName subOld ->
                    let res = subOld |> ofDirTree (fun _ v -> v, Removed)
                    Map.add dirName res ds
                )
                ds

        DirDiffTree(ds, fs)

    let rec apply isDeleteNonExistentFile srcDir dstDir (DirDiffTree(dirs, files)) =
        let files =
            files
            |> Map.choose (fun fileName ((oldLastWriteTime, diffType) as v) ->
                match diffType with
                | NotChanged ->
                    Some v
                | Added
                | Changed ->
                    try
                        // TODO: make sure that the folder is created only once, and not for every changed file, as it is now
                        System.IO.Directory.CreateDirectory dstDir |> ignore
                    with
                        e -> printfn "%A\n%A" (srcDir, dstDir) e.Message

                    let srcFilePath = Path.combine srcDir fileName
                    let dstFilePath = Path.combine dstDir fileName

                    try
                        let dstFileInfo = System.IO.FileInfo dstFilePath
                        if dstFileInfo.Exists then
                            dstFileInfo.Attributes <- FileAttributes.setNotReadOnly dstFileInfo.Attributes

                        try
                            System.IO.File.Copy(srcFilePath, dstFilePath, true)
                        with e ->
                            printfn "%A\n%A" (srcFilePath, dstFilePath) e.Message
                    with e ->
                        printfn "%A\n%A" (srcFilePath, dstFilePath) e.Message

                    Some v

                | Removed ->
                    try
                        System.IO.Directory.CreateDirectory dstDir |> ignore
                    with
                        e -> printfn "%A\n%A" (srcDir, dstDir) e.Message

                    let srcFilePath = Path.combine srcDir fileName
                    let dstFilePath = Path.combine dstDir fileName
                    try
                        let dstFileInfo = System.IO.FileInfo dstFilePath
                        if dstFileInfo.Exists then
                            dstFileInfo.Attributes <- FileAttributes.setNotReadOnly dstFileInfo.Attributes

                        let deleteNonExistentFile () =
                            if isDeleteNonExistentFile then
                                if dstFileInfo.Exists then
                                    dstFileInfo.Delete()
                                None
                            else
                                Some v

                        /// Git работает так:
                        /// если рассматриваемый файл уже закоммичен,
                        /// а потом пользователь добавляет правило в .gitignore, которое отсеивает этот файл,
                        /// то Git его не удаляет.
                        ///
                        /// Эта функция именно так и работает.
                        let apply () =
                            let srcFileInfo = new System.IO.FileInfo(srcFilePath)

                            if srcFileInfo.Exists then
                                let newLastWriteTime = srcFileInfo.LastWriteTime
                                if newLastWriteTime = oldLastWriteTime then
                                    Some v
                                else
                                    srcFileInfo.CopyTo(dstFilePath, true) |> ignore
                                    Some(newLastWriteTime, diffType)
                            else
                                None

                        deleteNonExistentFile ()

                    with e ->
                        printfn "%A\n%A" (srcFilePath, dstFilePath) e.Message
                        Some v
            )

        let dirs =
            dirs
            |> Map.map (fun dirName ->
                let src = Path.combine srcDir dirName
                let dst = Path.combine dstDir dirName

                apply isDeleteNonExistentFile src dst
            )

        DirDiffTree(dirs, files)
