module NederlandsEngels.Program

open System
open System.IO
open System.Text.RegularExpressions
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
    Regex.Replace(s, @"\s+", " ").Trim()
    
let parseEn (html : IHtmlElement) =
    html
        .QuerySelectorAll("span, p")
        .filter(fun e -> e.Children.Length = 0)
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
    let en = loadHtml @"../../../../Data/en/1.xhtml" |> parseEn
    let nl = loadHtml @"../../../../Data/nl/1.xhtml" |> parseNl
    0
