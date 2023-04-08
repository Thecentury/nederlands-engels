module NederlandsEngels.Program

open System.IO
open FSharp.Core.Fluent
open EpubParsing

(*--------------------------------------------------------------------------------------------------------------------*)

[<EntryPoint>]
let main _ =
    let root =
        if Directory.Exists "../Data" then
            // When running using 'dotnet run'
            "../Data"
        else
            // When running from the 'bin' folder
            @"../../../../Data"

    let en = loadHtml (Path.Combine (root, "en/1.xhtml")) |> parseEn
    let nl = loadHtml (Path.Combine (root, "nl/1.xhtml")) |> parseNl

    let enSentences = en |> String.concat " " |> SentenceParsing.splitIntoSentences
    let nlSentences = nl |> String.concat " " |> SentenceParsing.splitIntoSentences

    let zipped = enSentences.toSeq().zip(nlSentences)

    UI.Model.run (enSentences.toList()) (nlSentences.toList())

    // for en, nl in zipped do
    //     printfn $"%s{en}"
    //     printfn $"%s{nl}"
    //     printfn ""

    // printfn $"EN:\n\n%s{enSentences}"
    // printfn "\n\n"
    // printfn $"NL:\n\n%s{nlSentences}"

    0
