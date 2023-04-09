namespace rec NederlandsEngels

[<Struct>]
type Lens<'o, 'v> = Lens of ('o -> 'v) * ('o -> 'v -> 'o) with
    member this.Get = Lens.get this
    member this.Set = Lens.set this

module Lens =

    let create get set = Lens (get, set)

    let get (Lens (get, _)) = get

    let set (Lens (_, set)) = set

    let modify (Lens (get, set)) f o = set o (f (get o))

    let tryModify (Lens (get, set)) o f =
        match f (get o) with
        | Some v -> set o v
        | None -> o

    let map (Lens (get, set)) f = Lens (get, fun o v -> set o (f v))

    let compose (Lens (get1, set1)) (Lens (get2, set2)) =
        Lens (get1 >> get2, fun o v -> set1 o (set2 (get1 o) v))