namespace Backuper.Core.Git
open FsharpMyExtension
open FsharpMyExtension.Either

[<Struct>]
type RuleType =
    /// правило с двойной звездочкой
    | All
    /// то самое со всякими `?`, `*` и `[]`
    /// Двойной звездочки тут конечно же нет.
    | Mask of string

type Rule =
    {
        /// то правило, которое без `!` в начале.
        IsInclude: bool
        Body: RuleType list
    }
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Rule =
    let empty: Rule =
        {
            IsInclude = false
            Body = []
        }

    module Parser =
        open FParsec

        let inline prule< ^U> : Parser<Rule, ^U> =
            let mask =
                many1Satisfy (fun x -> x <> '/' && x <> '\n') <?> "mask"

            let p =
                let subRules =
                    (pstring "**" >>% All) <|> (mask |>> Mask)

                let pathSep = pchar '/'

                let p =
                    pipe3
                        subRules
                        (many (pathSep >>? subRules))
                        (opt pathSep .>> spaces)
                        (fun y xs ->
                            Option.map (fun x -> y::(xs @ [Mask "*"]))
                            >> Option.defaultValue (y::xs)
                        )

                (pathSep >>. p)
                <|> (((pstring "**/" >>. p) <|> p) |>> fun xs -> All::xs)

            (pchar '!' >>. p |>> fun xs -> { IsInclude = false; Body = xs})
            <|> (p |>> fun xs -> { IsInclude = true; Body = xs})

    /// Правило .gitignore напоминает регулярное выражение.
    /// Если применять его на полный путь, выглядеть будет так:
    /// * `?` -> `[^/]`
    /// * `*` -> `[^/]*?` оно же не жадное, да?
    /// * `.` -> `\.`
    /// * `[]` -> так и будет
    /// * `**` -> `.*?` наверное
    let toRegexPattern (rule: Rule) =
        let toRegexPattern =
            List.map (function
                | All -> ".*?"
                | Mask xs ->
                    xs
                    |> String.collect (
                        function
                        | '.' -> "\\."
                        | '?' -> "[^\\\\]"
                        | '*' -> "[^\\\\]*?"
                        | x -> string x
                    )
            )
            >> String.concat "\\\\"

        let regexPattern =
            match List.head rule.Body with
            | All -> toRegexPattern rule.Body
            | _ ->
                "\\G\\\\" + toRegexPattern rule.Body

        if rule.IsInclude then Right regexPattern else Left regexPattern

[<RequireQualifiedAccessAttribute>]
type Statement =
    | Rule of Rule
    | Comment of string
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module Statement =
    module Parser =
        open FParsec

        let inline pcomment< ^U> : Parser<_, ^U> =
            pchar '#' >>. manySatisfy ((<>) '\n')

        let inline pstatement< ^U> : Parser<_, ^U> =
            (pcomment |>> Statement.Comment)
            <|> (Rule.Parser.prule |>> Statement.Rule)

    let isRule (statement: Statement) =
        match statement with
        | Statement.Rule _ -> true
        | _ -> false

    let tryGetRule (statement: Statement) =
        match statement with
        | Statement.Rule r -> Some r
        | _ -> None

type StatementsList = Statement list
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module StatementsList =
    module Parser =
        open FParsec

        let inline prules< ^U> : Parser<StatementsList, ^U> =
            spaces
            >>. many (
                Statement.Parser.pstatement
                .>> spaces
            )

        let parse input =
            match run (prules .>> eof) input with
            | Success(x, _, _) -> x
            | x -> failwithf "%A" x

        let parseFile path =
            match runParserOnFile (prules .>> eof) () path System.Text.Encoding.UTF8 with
            | Success(x, _, _) -> x
            | x -> failwithf "%s\n%A" path x

    let parse input =
        Parser.parse input

    let parseFile path =
        Parser.parseFile path

/// `includes * excludes`
type IncludesExcludes =
    System.Text.RegularExpressions.Regex list *
    System.Text.RegularExpressions.Regex list
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module IncludesExcludes =
    let parseFile path : IncludesExcludes =
        let includes, excludes =
            StatementsList.parseFile path
            |> List.choose Statement.tryGetRule
            |> List.map Rule.toRegexPattern
            |> List.partitionEithers

        (includes, excludes)
        |> mapBoth (
            List.map (fun patt ->
                System.Text.RegularExpressions.Regex(patt, System.Text.RegularExpressions.RegexOptions.IgnoreCase)
            )
        )

/// `includes * excludes`
type GitIgnore = ((string -> bool) * (string -> bool))
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module GitIgnore =
    let empty =
        (fun _ -> true), (fun _ -> false)

    let create pathStartAt gitignorePath : GitIgnore =
        let includes, excludes = IncludesExcludes.parseFile gitignorePath

        let isMatch startAt input =
            List.exists (fun (r: System.Text.RegularExpressions.Regex) ->
                r.IsMatch(input, startAt)
            )

        let excludesf input = isMatch pathStartAt input excludes
        let includesf input = isMatch pathStartAt input includes

        includesf, excludesf

    let isMatch path ((includes, excludes): GitIgnore) =
        not (excludes path) || includes path
