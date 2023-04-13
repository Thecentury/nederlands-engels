module NederlandsEngels.EpubParsing

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

let private normalizeSpaces (s : string) =
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
        .ofType<INode>()
        .collect(fun e -> e.ChildNodes)
        .ofType<IText>()
        .map(fun e -> normalizeSpaces e.TextContent)
        .filter(not << String.IsNullOrWhiteSpace)
        .toList()