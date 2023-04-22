namespace NederlandsEngels

open FSharp.Core.Fluent

(*--------------------------------------------------------------------------------------------------------------------*)

type Zipper<'a> = {
  Left : List<'a>
  Focus : 'a
  Right : List<'a>
}

module Zipper =

  let fromList (l : List<'a>) =
    match l with
    | [] -> failwith "Cannot create zipper from empty list"
    | h :: t -> { Left = []; Focus = h; Right = t }

  let tryFromList (l : List<'a>) =
    match l with
    | [] -> None
    | h :: t -> Some { Left = []; Focus = h; Right = t }

  let fromCons focus rights =
    { Left = []; Focus = focus; Right = rights }

  let toList (z : Zipper<'a>) =
    z.Left.reverse() @ [z.Focus] @ z.Right

  let tryMoveLeft (z : Zipper<'a>) =
    match z.Left with
    | [] -> None
    | h :: t -> Some { Left = t; Focus = h; Right = z.Focus :: z.Right }

  let tryMoveRight (z : Zipper<'a>) =
    match z.Right with
    | [] -> None
    | h :: t -> Some { Left = z.Focus :: z.Left; Focus = h; Right = t }

  let selfAndRights (z : Zipper<'a>) =
    Seq.unfold (function
      | None -> None
      | Some z -> Some (z, tryMoveRight z))
      ^ Some z

  /// Moves in a zipper using the 'tryMove' function until the condition 'f' is true for the focus. Returns None if the
  /// condition hasn't become true and the movement is no longer possible.
  let rec tryMoveUntil f tryMove (z : Zipper<'a>) =
    if f z.Focus then
      Some z
    else
      z |> tryMove |> Option.bind (tryMoveUntil f tryMove)

  /// Moves in a zipper using the 'tryMove' function while the condition 'f' is true for the focus. Returns 'Some' when
  /// after a movement the condition is not true. Returns None if the condition is not true but the movement is no
  /// longer possible.
  let rec tryMoveWhile f tryMove (z : Zipper<'a>) =
    if f z.Focus then
      z |> tryMove |> Option.bind (tryMoveWhile f tryMove)
    else
      Some z

  let enumerateOutOfOrder (z : Zipper<'a>) = seq {
    yield! z.Left
    yield z.Focus
    yield! z.Right
  }

  let (|Zipper|) z =
    (z.Left, z.Focus, z.Right)
