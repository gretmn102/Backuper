namespace Backuper
type FileName = string
type DirName = string
type LastWriteTime = System.DateTime
type DirTree = DirTree of Map<DirName,DirTree> * Map<FileName, LastWriteTime>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module DirTree =
    open FsharpMyExtension

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
