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
            if c = abbr[index] then
                isAbbreviation abbr (index - 1) rest
            else
                false
                    
    abbreviations |> Array.exists (fun abbr -> isAbbreviation abbr (abbr.Length - 1) sentence)

let private completeSentence = List.rev >> List.toArray >> String >> Some

type Input =
    | Char of char
    | EndOfInput

// todo triple dots

let trimStartingSpaces previous = M.machine ^ fun c ->
    match previous, c with
    | None, EndOfInput -> Transition (None, M.halt ())
    | Some ' ', Char ' '
    | None, Char ' ' -> Transition (None, trimStartingSpaces None)
    | Some ' ', Char c -> Transition (None, normal [c])
    | Some prev, EndOfInput -> Transition (Some ^ String [| prev |], M.halt())
    | Some prev, Char c -> Transition (None, normal [c; prev])
    | None, Char c -> Transition (None, normal [c])
    
let continueConsumingAfterDots soFar = M.machine ^ fun c ->
    match c with
    | EndOfInput ->
        Transition (completeSentence soFar, M.halt ())
    | Char c when Char.IsWhiteSpace c ->
        Transition (None, continueConsumingAfterDots (c :: soFar))
    | Char c when Char.IsDigit c || Char.IsLower c ->
        Transition (None, normal (c :: soFar))
    | Char _ ->
        Halt
        // Transition (completeSentence soFar, trimStartingSpaces (Some c))

let consumeDots soFar = M.machine ^ fun c ->
    match c with
    | EndOfInput ->
        Transition (completeSentence soFar, M.halt ())
    | Char ('.' as c) ->
        Transition (None, consumeDots (c :: soFar))
    | Char c ->
        // todo here soFar is lost in the second case.
        Transition (None, continueConsumingAfterDots (c :: soFar) <|> trimStartingSpaces (Some c))
    
let consumeTerminators soFar = M.machine ^ fun (c : Input) ->
    match c with
    | EndOfInput ->
        Transition (completeSentence soFar, M.halt ())
    | Char (SentenceTerminator as c) ->
        Transition (None, consumeTerminators (c :: soFar))
    | Char c ->
        Transition (completeSentence soFar, trimStartingSpaces (Some c))

let normal (soFar : List<char>) = M.machine ^ fun c ->
    match c with
    | EndOfInput ->
        Transition (completeSentence soFar, M.halt ())
    | Char c ->
        let sentence = c :: soFar
        match c with
        | '"' ->
            Transition (None, (insideDoubleQuotes sentence c))
        | ''' ->
            Transition (None, (insideSingleQuotes sentence c))
        | SentenceTerminatorNotDot ->
            Transition (None, consumeTerminators sentence)
        | '.' when not ^ isAbbreviation soFar ->
            Transition (None, consumeDots sentence)
        | _ -> Transition (None, normal sentence)
        
let insideDoubleQuotes soFar quote = M.machine ^ fun c ->
    match c with
    | EndOfInput ->
        // todo mikbri Strictly speaking, quotes are not closed.
        Transition (completeSentence soFar, M.halt ())
    | Char c ->
        let sentence = c :: soFar
        match c, soFar with
        | Equals quote, StringEndsWithTerminator ->
            Transition (completeSentence sentence, normal [])
        | Equals quote, _ ->
            Transition (None, normal sentence)
        | _ ->
            Transition (None, insideDoubleQuotes sentence quote)
        
let insideSingleQuotes soFar quote = M.machine ^ fun c ->
    match c with
    | EndOfInput ->
        // todo mikbri Strictly speaking, quotes are not closed.
        Transition (completeSentence soFar, M.halt ())
    | Char c ->
        let sentence = c :: soFar
        // todo detect usage as in "Boy's".
        match c, soFar with
        | Equals quote, StringEndsWithTerminator ->
            Transition (completeSentence sentence, normal [])
        | Equals quote, _ ->
            Transition (None, normal sentence)
        | _ ->
            Transition (None, insideSingleQuotes sentence quote)
        
let findSentences (s : string) =
    let mutable machine = trimStartingSpaces None
    let sentences = ResizeArray()
    
    let inputs =
        s.map(Char).append([EndOfInput])
    
    let mutable index = -1
    for i in inputs do
        index <- index + 1
        let trans = M.step i machine
        match trans with
        | Halt -> failwith $"Unexpected end of sentence at position {index + 1}: {s.Substring(index)}"
        | Transition (sentence, nextMachine) ->
            machine <- nextMachine
            match sentence with
            | Some sentence -> sentences.Add sentence
            | None -> ()
    
    sentences.ToArray()