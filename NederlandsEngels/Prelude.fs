[<AutoOpen>]
module NederlandsEngels.Prelude

let inline (^) f x = f x

let inline constant x = fun _ -> x
