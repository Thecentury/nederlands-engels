module rec NederlandsEngels.SentenceParsing

open System

open Machines
open SeqMachines
open Text

module M = Machine

#nowarn "40"

(*--------------------------------------------------------------------------------------------------------------------*)

module private Char =

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

let private restoreSentence = List.rev >> List.toArray >> String
let private completeSentence = restoreSentence >> List.singleton
let private noSentences = List.empty

// todo triple dots

let private trimStartingSpaces sentenceToYield previous = M.machine ^ fun c ->
    let sentenceToYield = sentenceToYield |> Option.map restoreSentence |> Option.toList
    match previous, c with
    | None, EndOfInput -> Transition (sentenceToYield, M.halt ())
    | Some ' ', Element ' '
    | None, Element ' ' -> Transition (sentenceToYield, trimStartingSpaces None None)
    | Some ' ', Element c -> Transition (sentenceToYield, sentenceLetter [] c)
    | Some prev, EndOfInput ->
        let sentences = String [| prev |] :: sentenceToYield
        Transition (sentences, M.halt())
    | Some prev, Element c -> Transition (sentenceToYield, normal [c; prev])
    | None, Element c -> Transition (sentenceToYield, sentenceLetter [] c)

let private continueConsumingAfterI soFar = M.machine ^ function
    | EndOfInput ->
        Transition (completeSentence soFar, M.halt ())
    | Element c when Char.IsWhiteSpace c ->
        Transition (noSentences, normal (c :: soFar))
    | Element _ -> Halt

let private continueConsumingAfterPossibleSentenceEnd soFar = M.machine ^ function
    | EndOfInput ->
        Transition (completeSentence soFar, M.halt ())
    | Element c when Char.IsWhiteSpace c ->
        Transition (noSentences, continueConsumingAfterPossibleSentenceEnd (c :: soFar))
    | Element c when Char.IsDigit c || Char.IsLower c ->
        Transition (noSentences, normal (c :: soFar))
    | Element ('I' as c) ->
        Transition (noSentences, continueConsumingAfterI (c :: soFar))
    | Element _ ->
        Halt

let private appendSentence (sentence : List<'a>) (sentences : List<List<'a>>) : List<'a> = (sentences @ [sentence]) |> List.collect id

let private tryFirst first second =
    M.tryFirst (List.isEmpty >> not) [] appendSentence first second

let private consumeDots soFar = M.machine ^ function
    | EndOfInput ->
        Transition (completeSentence soFar, M.halt ())
    | Element ('.' as c) ->
        Transition (noSentences, consumeDots (c :: soFar))
    | Element c ->
        Transition (completeSentence soFar, trimStartingSpaces None (Some c))

let private consumeTerminators soFar = M.machine ^ fun (c : Input<char>) ->
    match c with
    | EndOfInput ->
        Transition (completeSentence soFar, M.halt ())
    | Element (SentenceTerminator as c) ->
        Transition (noSentences, consumeTerminators (c :: soFar))
    | Element c ->
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
    | Element c ->
        Transition (noSentences, sentenceLetter soFar c)

let private insideDoubleQuotes soFar quote = M.machine ^ fun c ->
    match c with
    | EndOfInput ->
        // todo mikbri Strictly speaking, quotes are not closed.
        Transition (completeSentence soFar, M.halt ())
    | Element c ->
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
    | Element c ->
        let sentence = c :: soFar
        match c, soFar with
        | Equals quote, StringEndsWithTerminator ->
            // 'A?' said B. - after the "?'" the sentence does not end.
            let next = tryFirst
                           (continueConsumingAfterPossibleSentenceEnd sentence)
                           (trimStartingSpaces (Some sentence) None)
            Transition (noSentences, next)
        | Equals quote, _ ->
            Transition (noSentences, normal sentence)
        | _ ->
            Transition (noSentences, insideSingleQuotes sentence quote)

let splitIntoSentences (s : string) =
    applyToCharacters s (trimStartingSpaces None None) |> List.collect id |> List.toArray