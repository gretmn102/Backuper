module GitIgnore
open FsharpMyExtension
open FsharpMyExtension.Either

[<Struct>]
type Typ =
    /// правило с двойной звездочкой
    | All
    /// то самое со всякими `?`, `*` и `[]`
    /// Двойной звездочки тут конечно же нет.
    | Mask of string

type Rule =
    {
        /// то правило, которое без `!` в начале.
        Include:bool
        Body : Typ list
    }

module Parser =
    open FParsec

    let comment : Parser<unit, unit> =
        pchar '#' >>. skipManySatisfy ((<>) '\n') >>. spaces
    let rule : Parser<Rule, unit> =
        let mask : Parser<string,unit> =
            many1Satisfy (fun x -> x <> '/' && x <> '\n') <?> "mask"

        let subrules =
            (pstring "**" >>% All) <|> (mask |>> Mask)

        let p =
            let sep = pchar '/'
            let p =
                pipe3
                    subrules
                    (many (sep >>? subrules))
                    (opt sep .>> spaces)
                    (fun y xs ->
                        Option.map (fun x -> y::(xs @ [Mask "*"]))
                        >> Option.defaultValue (y::xs) )
            (sep >>. p) <|> ((pstring "**/" >>. p) <|> p |>> fun xs -> All::xs)

        (pchar '!' >>. p |>> fun xs -> { Include = false; Body = xs})
        <|> (p |>> fun xs -> { Include = true; Body = xs})
        .>> spaces

    let par = spaces >>. many ((many comment >>. rule) <|> rule)

    let start str =
        match run (par .>> eof) str with
        | Success(x, _, _) -> x
        | x -> failwithf "%A" x

    let startFile path =
        match runParserOnFile (par .>> eof) () path System.Text.Encoding.UTF8 with
        | Success(x, _, _) -> x
        | x -> failwithf "%s\n%A" path x

/// Правило .gitignore напоминает регулярное выражение.
/// Если применять его на полный путь, выглядеть будет так:
/// * `?` -> `[^/]`
/// * `*` -> `[^/]*?` оно же не жадное, да?
/// * `.` -> `\.`
/// * `[]` -> так и будет
/// * `**` -> `.*?` наверное
let ruleToRegexPattern (x:Rule) =
    let toRegex =
        List.map (
            function
            | All -> ".*?"
            | Mask xs ->
                xs |> String.collect (
                    function
                    | '.' -> "\\."
                    | '?' -> "[^\\\\]"
                    | '*' -> "[^\\\\]*?"
                    | x -> string x
                )
        )
        >> String.concat "\\\\"

    let r =
        match List.head x.Body with
        | All -> toRegex x.Body
        | _ ->
            "\\G\\\\" + toRegex x.Body

    if x.Include then Right r else Left r

let isMatch input patt =
    System.Text.RegularExpressions.Regex.IsMatch(input, patt, System.Text.RegularExpressions.RegexOptions.IgnoreCase)

/// `includes * excludes`
type GitignoreRules =
    System.Text.RegularExpressions.Regex list *
    System.Text.RegularExpressions.Regex list

let gitignoreLoad path : GitignoreRules =
    let includes, excludes =
        Parser.startFile path
        |> List.map ruleToRegexPattern
        |> List.partitionEithers

    let includes, excludes =
        (includes, excludes)
        |> mapBoth (List.map (fun patt ->
            System.Text.RegularExpressions.Regex(patt, System.Text.RegularExpressions.RegexOptions.IgnoreCase)))
    includes, excludes
