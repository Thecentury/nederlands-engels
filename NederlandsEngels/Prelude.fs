[<AutoOpen>]
module NederlandsEngels.Prelude

open System
open System.Runtime.CompilerServices

let inline (^) f x = f x

let inline constant x = fun _ -> x

open System.Collections

[<Extension>]
type SeqExtensions () =

  [<Extension>]
  static member ofType<'b> (s : IEnumerable) : seq<'b> =
    s
    |> Seq.cast<obj>
    |> Seq.choose (function | :? 'b as x -> Some x | _ -> None)

(*--------------------------------------------------------------------------------------------------------------------*)

module List =

  let withNext list =
    let fold current next = ((current, next), Some current)
    let pairs, _ = List.mapFoldBack fold list None
    pairs

(*--------------------------------------------------------------------------------------------------------------------*)

module Disposable =

  let ofFunction f =
    { new IDisposable with
      member x.Dispose() = f() }