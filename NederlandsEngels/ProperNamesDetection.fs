module rec NederlandsEngels.ProperNamesDetection

open System

open Machines

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
    equalityComparer.Equals(this.Value, other.Value) &&
    equalityComparer.Equals(this.Category, other.Category)

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

let private isBeginningOfAName z =
  match z.Focus, z.Left with
  | c, left :: otherLeft when Char.IsUpper(c) && left = ' ' ->
    let skipSpaces = otherLeft |> List.skipWhile ((=) ' ')
    match skipSpaces with
    | [] -> false
    | c :: _ when Char.IsLetterOrDigit c || List.contains c [','; '-'; '''; '"'] -> true
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