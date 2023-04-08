module NederlandsEngels.Gui.Model

open Elmish
open FSharp.Core.Fluent
open NederlandsEngels

(*--------------------------------------------------------------------------------------------------------------------*)

type Selection =
    | English
    | Dutch

type Entry = {
    English : List<string>
    Dutch : List<string>
    Selected : Option<Selection>
}

let private mkEntry (en : Option<string>) (nl : Option<string>) =
    match en, nl with
    | Some en, Some nl -> Some { English = [en]; Dutch = [nl]; Selected = None }
    | Some en, None -> Some { English = [en]; Dutch = []; Selected = None }
    | None, Some nl -> Some { English = []; Dutch = [nl]; Selected = None }
    | None, None -> None

type Model = {
    Entries : List<Entry>
}

type Msg = unit

let init (en : List<string>) (nl : List<string>) =
    let en = en.map(Some).append(Seq.initInfinite (constant None))
    let nl = nl.map(Some).append(Seq.initInfinite (constant None))
    let entries =
        en.zip(nl)
        |> Seq.map (fun (en, nl) -> mkEntry en nl)
        |> Seq.takeWhile Option.isSome
        |> Seq.choose id
        |> Seq.toList
    // todo load from a state file
    let model = {
        Entries = entries
    }
    model, Cmd.none

let update msg model = model, Cmd.none