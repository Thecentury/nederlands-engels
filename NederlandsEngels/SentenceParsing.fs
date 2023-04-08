module rec NederlandsEngels.SentenceParsing

open System
open FSharp.Core.Fluent

open NederlandsEngels.Machines

module M = Machine

#nowarn "40"

(*--------------------------------------------------------------------------------------------------------------------*)

module Char =
    
    let isQuote = function
        | '"' | ''' -> true
        | _ -> false
        
(*--------------------------------------------------------------------------------------------------------------------*)

let private (|Equals|_|) example value =
    if value = example then
        Some ()
    else
        None
        
let private (|StringEndsWithTerminator|_|) = function
    | '.' :: _
    | '?' :: _
    | '!' :: _
    | ' ' :: '.' :: _
    | ' ' :: '?' :: _
    | ' ' :: '!' :: _ -> Some ()
    | _ -> None
    
let private (|SentenceTerminator|_|) = function
    | '.' | '?' | '!' -> Some ()
    | _ -> None
    
let private (|SentenceTerminatorNotDot|_|) = function
    | '?' | '!' -> Some ()
    | _ -> None
    
// Dr., Mr., Mrs., etc. + all caps

let private abbreviations = [|
        "mr"
        "mrs"
        "ms"
        "dr"
        "prof"
        "sr"
        "jr"
        "st"
        "rev"
    |]

let private isAbbreviation (sentence : List<char>) =
    let rec isAbbreviation (abbr : string) index sentence =
        match index, sentence with
        | -1, _ -> true
        | _, [] -> false // The sentence is too short to be an abbreviation
        | _, c :: rest ->
            if Char.ToLowerInvariant c = abbr[index] then
                isAbbreviation abbr (index - 1) rest
            else
                false
                    
    abbreviations |> Array.exists (fun abbr -> isAbbreviation abbr (abbr.Length - 1) sentence)

let private restoreSentence = List.rev >> List.toArray >> String
let private completeSentence = restoreSentence >> List.singleton
let private noSentences = List.empty

type private Input =
    | Char of char
    | EndOfInput

// todo triple dots

let private trimStartingSpaces sentenceToYield previous = M.machine ^ fun c ->
    let sentenceToYield = sentenceToYield |> Option.map restoreSentence |> Option.toList
    match previous, c with
    | None, EndOfInput -> Transition (sentenceToYield, M.halt ())
    | Some ' ', Char ' '
    | None, Char ' ' -> Transition (sentenceToYield, trimStartingSpaces None None)
    | Some ' ', Char c -> Transition (sentenceToYield, sentenceLetter [] c)
    | Some prev, EndOfInput ->
        let sentences = String [| prev |] :: sentenceToYield
        Transition (sentences, M.halt())
    | Some prev, Char c -> Transition (sentenceToYield, normal [c; prev])
    | None, Char c -> Transition (sentenceToYield, sentenceLetter [] c)
    
let private continueConsumingAfterI soFar = M.machine ^ function
    | EndOfInput ->
        Transition (completeSentence soFar, M.halt ())
    | Char c when Char.IsWhiteSpace c ->
        Transition (noSentences, normal (c :: soFar))
    | Char _ -> Halt
    
let private continueConsumingAfterPossibleSentenceEnd soFar = M.machine ^ function
    | EndOfInput ->
        Transition (completeSentence soFar, M.halt ())
    | Char c when Char.IsWhiteSpace c ->
        Transition (noSentences, continueConsumingAfterPossibleSentenceEnd (c :: soFar))
    | Char c when Char.IsDigit c || Char.IsLower c ->
        Transition (noSentences, normal (c :: soFar))
    | Char ('I' as c) ->
        Transition (noSentences, continueConsumingAfterI (c :: soFar))
    | Char _ ->
        Halt

let private appendSentence (sentence : List<'a>) (sentences : List<List<'a>>) : List<'a> = (sentences @ [sentence]) |> List.collect id

let private tryFirst first second =
    M.tryFirst (List.isEmpty >> not) [] appendSentence first second

let private consumeDots soFar = M.machine ^ function
    | EndOfInput ->
        Transition (completeSentence soFar, M.halt ())
    | Char ('.' as c) ->
        Transition (noSentences, consumeDots (c :: soFar))
    | Char c ->
        // let next = tryFirst (continueConsumingAfterPossibleSentenceEnd (c :: soFar)) (trimStartingSpaces (Some soFar) (Some c))
        // Transition (noSentences, next)
        Transition (completeSentence soFar, trimStartingSpaces None (Some c))
    
let private consumeTerminators soFar = M.machine ^ fun (c : Input) ->
    match c with
    | EndOfInput ->
        Transition (completeSentence soFar, M.halt ())
    | Char (SentenceTerminator as c) ->
        Transition (noSentences, consumeTerminators (c :: soFar))
    | Char c ->
        Transition (completeSentence soFar, trimStartingSpaces None (Some c))

let private sentenceLetter soFar c =
    let endsWithLetter =
        match soFar with
        | [] -> false
        | c :: _ -> Char.IsLetter c
    let sentence = c :: soFar
    match c with
    | '"' ->
        insideDoubleQuotes sentence c
    | ''' when endsWithLetter ->
        // It's an apostrophe, not a quote.
        normal sentence
    | ''' ->
        insideSingleQuotes sentence c
    | SentenceTerminatorNotDot ->
        consumeTerminators sentence
    | '.' when not ^ isAbbreviation soFar ->
        consumeDots sentence
    | _ -> normal sentence

let private normal (soFar : List<char>) = M.machine ^ fun c ->
    match c with
    | EndOfInput ->
        Transition (completeSentence soFar, M.halt ())
    | Char c ->
        Transition (noSentences, sentenceLetter soFar c)
        
let private insideDoubleQuotes soFar quote = M.machine ^ fun c ->
    match c with
    | EndOfInput ->
        // todo mikbri Strictly speaking, quotes are not closed.
        Transition (completeSentence soFar, M.halt ())
    | Char c ->
        let sentence = c :: soFar
        match c, soFar with
        | Equals quote, StringEndsWithTerminator ->
            // "A?" said B. - after the "?"" the sentence does not end.
            let next = tryFirst
                           (continueConsumingAfterPossibleSentenceEnd sentence)
                           (trimStartingSpaces (Some sentence) None)
            Transition (noSentences, next)
        | Equals quote, _ ->
            Transition (noSentences, normal sentence)
        | _ ->
            Transition (noSentences, insideDoubleQuotes sentence quote)

// todo maintain a stack of quotes to allow nesting?
let private insideSingleQuotes soFar quote = M.machine ^ fun c ->
    match c with
    | EndOfInput ->
        // todo mikbri Strictly speaking, quotes are not closed.
        Transition (completeSentence soFar, M.halt ())
    | Char c ->
        let sentence = c :: soFar
        match c, soFar with
        | Equals quote, StringEndsWithTerminator ->
            // 'A?' said B. - after the "?'" the sentence does not end.
            let next = tryFirst (continueConsumingAfterPossibleSentenceEnd sentence) (trimStartingSpaces (Some sentence) None)
            Transition (noSentences, next)
        | Equals quote, _ ->
            // todo detect usage as in "Boy's".
            Transition (noSentences, normal sentence)
        | _ ->
            Transition (noSentences, insideSingleQuotes sentence quote)
        
let splitIntoSentences (s : string) =
    let mutable machine = trimStartingSpaces None None
    let sentences = ResizeArray()
    
    let inputs =
        s.map(Char).append([EndOfInput])
    
    let mutable index = -1
    for i in inputs do
        index <- index + 1
        let trans = M.step i machine
        match trans with
        | Halt ->
            let str = $"{s.Substring(0, index)}{s.Substring(index)}"
            let pointer = String.replicate index " " + "^"
            let msg = $"'Unexpected end of sentence at position {index + 1} ('{i}'):\n{str}\n{pointer}"
            failwith msg
        | Transition (newSentences, nextMachine) ->
            machine <- nextMachine
            sentences.AddRange newSentences
    
    sentences.ToArray()