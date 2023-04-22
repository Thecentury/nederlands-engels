module NederlandsEngels.Text

open System

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

let isAbbreviation (sentence : List<char>) =
  let rec isAbbreviation (abbr : string) index sentence =
    match index, sentence with
    | -1, [] -> true // The sentence was starting from the abbreviation
    | -1, ' ' :: _ -> true // The abbreviation is fully consumed, and there is a space before it
    | -1, _ -> false // The abbreviation is fully consumed, but the word in the sentence continues.
    | _, [] -> false // The sentence is too short to be an abbreviation
    | _, c :: rest ->
      if Char.ToLowerInvariant c = abbr[index] then
        isAbbreviation abbr (index - 1) rest
      else
        false

  abbreviations |> Array.exists (fun abbr -> isAbbreviation abbr (abbr.Length - 1) sentence)