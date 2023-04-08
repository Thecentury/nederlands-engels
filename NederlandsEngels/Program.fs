module NederlandsEngels.Program

open System
open System.IO
open System.Text.RegularExpressions
open AngleSharp.Dom
open AngleSharp.Html.Dom
open FSharp.Core.Fluent
open AngleSharp
open AngleSharp.Io

(*--------------------------------------------------------------------------------------------------------------------*)

let loadHtml (path : string) =
    let xml = File.ReadAllText(path)
    let ctx = BrowsingContext.New(Configuration.Default.WithDefaultLoader())
    let html = ctx.OpenAsync(Action<VirtualResponse>(fun req -> req.Content(xml) |> ignore)).Result
    html.Body
    
let normalizeSpaces (s : string) =
    let normalized = Regex.Replace(s, @"\s+", " ").Trim()
    normalized
    
let private isNotLink (e : IElement) =
    e.ClassName <> "calibre3"
    
let parseEn (html : IHtmlElement) =
    html
        .QuerySelectorAll("span")
        .filter(isNotLink)
        .ofType<INode>()
        .collect(fun e -> e.ChildNodes)
        .ofType<IText>()
        .map(fun e -> normalizeSpaces e.TextContent)
        .filter(not << String.IsNullOrWhiteSpace)
        .toList()
        
let parseNl (html : IHtmlElement) =
    html.QuerySelectorAll("span, p, h2, h3")
        .filter(fun e -> e.Children.forall(function | :? IHtmlBreakRowElement -> true | _ -> false))
        .map(fun e -> normalizeSpaces e.TextContent)
        .filter(not << String.IsNullOrWhiteSpace)
        .toList()

[<EntryPoint>]
let main _ =
    let root =
        if Directory.Exists "../Data" then
            // When running using 'dotnet run'
            "../Data"
        else
            // When running from the 'bin' folder
            @"../../../../Data"

    // let en = loadHtml (Path.Combine (root, "en/1.xhtml")) |> parseEn
    let nl = loadHtml (Path.Combine (root, "nl/1.xhtml")) |> parseNl
    
    // let enSentences = en |> String.concat " " |> SentenceParsing.splitIntoSentences |> String.concat "\n\n"
    let nlSentences = nl |> String.concat " " |> SentenceParsing.splitIntoSentences |> String.concat "\n\n"
    
    // printfn $"EN:\n\n%s{enSentences}"
    // printfn "\n\n"
    printfn $"NL:\n\n%s{nlSentences}"
    
    0
