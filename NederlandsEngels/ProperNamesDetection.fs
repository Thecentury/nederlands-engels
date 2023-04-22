module rec NederlandsEngels.ProperNamesDetection

open System

open Machines
open Zipper

module M = Machine

(*--------------------------------------------------------------------------------------------------------------------*)

let inline private equalityComparer<'a> =
  System.Collections.Generic.EqualityComparer<'a>.Default

[<Struct>]
type TextCategory =
  | RegularText
  | ProperName

[<CustomEquality>]
[<NoComparison>]
type AnnotatedValue<'a> = {
  Value : 'a
  Category : TextCategory
} with
  override this.Equals(obj) =
    match obj with
    | :? AnnotatedValue<'a> as other -> this.Equals other
    | _ -> false

  member this.Equals (other : AnnotatedValue<'a>) =
    equalityComparer.Equals(this.Category, other.Category) &&
    equalityComparer.Equals(this.Value, other.Value)

  override this.GetHashCode() =
    HashCode.Combine(
      equalityComparer.GetHashCode(this.Value),
      this.Category.GetHashCode())

  interface IEquatable<AnnotatedValue<'a>> with
    member this.Equals other = this.Equals other

let private mapValue (f : 'a -> 'b) (annotated : AnnotatedValue<'a>) =
  { Value = f annotated.Value; Category = annotated.Category }

let private annotate value category = { Value = value; Category = category }

type private Input = Zipper<char>

(*--------------------------------------------------------------------------------------------------------------------*)

let private (|IsUpper|_|) (c : char) =
  if Char.IsUpper c then
    Some c
  else
    None

let private isBeginningOfAName (z : Input) =
  let isAbbreviation focus right =
    let z = Zipper.fromCons focus right
    // Move the zipper to the right while the char is a letter.
    let maybeAfterTheWord = z |> Zipper.tryMoveWhile Char.IsLetter Zipper.tryMoveRight
    match maybeAfterTheWord with
    | None -> false
    // After skipping all letters the next character is not a dot.
    | Some (Zipper (_, focus, _)) when focus <> '.' -> false
    // The abbreviation is at the end of the string.
    | Some (Zipper (left, _dot, [])) when Text.isAbbreviation left -> true
    // There is a space after the abbreviation.
    | Some (Zipper (left, _dot, ' ' :: _)) when Text.isAbbreviation left -> true
    // The abbreviation ends with a single or double quote
    | Some (Zipper (left, _dot, ('"' | ''') :: _)) when Text.isAbbreviation left -> true
    // The string is not over after the dot and the next symbol is not a space.
    | _ -> false
  match z with
  | Zipper (' ' :: _, 'I', ' ' :: _) -> false
  | Zipper (' ' :: _, IsUpper c, right) when isAbbreviation c right -> false
  | Zipper (' ' :: otherLeft, IsUpper _, _) ->
    let skipSpaces = otherLeft |> List.skipWhile ((=) ' ')
    match skipSpaces with
    | [] -> false // It may be just a beginning of a sentence.
    | '.' :: rest when Text.isAbbreviation rest -> true // A proper name goes after an abbreviation ending with a '.'.
    | c :: _ when Char.IsLetterOrDigit c || List.contains c [','; '-'; '''; '"'; '—'] -> true
    | _ -> false
  | _ -> false

let private analyzeLetter (z : Input) =
  if isBeginningOfAName z then
    Transition (annotate z.Focus ProperName, M.machine inName)
  else
    Transition (annotate z.Focus RegularText, M.machine analyzeLetter)

let private inName (z : Input) =
  match z.Focus with
  | c when Char.IsLetter c -> Transition (annotate c ProperName, M.machine inName)
  | c -> Transition (annotate c RegularText, M.machine analyzeLetter)

let private machine = M.machine analyzeLetter

(*--------------------------------------------------------------------------------------------------------------------*)

let private applyToCharacters (s : string) m =
    s
    |> Seq.toList
    |> Zipper.fromList
    |> Zipper.selfAndRights
    |> Seq.mapFold (fun (machine, index) char ->
        let trans = M.step char machine
        match trans with
        | Halt ->
            let str = $"{s.Substring(0, index)}{s.Substring(index)}"
            let pointer = String.replicate index " " + "^"
            let msg = $"'Unexpected end of sentence at position {index + 1} ('{char}'):\n{str}\n{pointer}"
            failwith msg
        | Transition (newSentences, nextMachine) ->
            newSentences, (nextMachine, index + 1)
      ) (m, 0)
    |> fst
    |> Seq.toList

let detectNames (s : string) =
  applyToCharacters s machine
  |> List.fold (fun (soFar : List<AnnotatedValue<List<char>>>) (char : AnnotatedValue<char>) ->
      match soFar with
      | [] -> [mapValue List.singleton char]
      | lastGroup :: rest ->
          if lastGroup.Category = char.Category then
            mapValue (fun prevChars -> char.Value :: prevChars) lastGroup :: rest
          else
            mapValue List.singleton char :: soFar
    ) []
  |> List.rev
  |> List.map (mapValue (List.rev >> List.toArray >> String))