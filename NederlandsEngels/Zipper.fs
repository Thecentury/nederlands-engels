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

    let enumerateOutOfOrder (z : Zipper<'a>) = seq {
      yield! z.Left
      yield z.Focus
      yield! z.Right
    }
