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
    // Selected : Option<Selection>
}

let private mkEntry (en : Option<string>) (nl : Option<string>) =
    match en, nl with
    | None, None -> None
    | en, nl -> Some { English = Option.toList en; Dutch = Option.toList nl }

type Model = {
    Entries : List<Entry>
    SelectedIndex : int
    Selection : Selection
}

type Msg =
    | MoveFocusLeft
    | MoveFocusRight
    | MoveFocusUp
    | MoveFocusDown
    | MergeUp

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
        SelectedIndex = 0
        Selection = English
    }
    model, Cmd.none

let update (msg : Msg) model =
    match msg with
    | MoveFocusUp ->
        let index = max 0 (model.SelectedIndex - 1)
        let model = { model with SelectedIndex = index }
        model, Cmd.none
    | MoveFocusDown ->
        let index = min (model.Entries.Length - 1) (model.SelectedIndex + 1)
        let model = { model with SelectedIndex = index }
        model, Cmd.none
    | MoveFocusLeft ->
        { model with Selection = English }, Cmd.none
    | MoveFocusRight ->
        { model with Selection = Dutch }, Cmd.none
    | MergeUp ->
        if model.SelectedIndex = 0 then
            model, Cmd.none
        else
            let selectedEntry = model.Entries |> List.item model.SelectedIndex
            let previousEntry = model.Entries |> List.item (model.SelectedIndex - 1)
            let textToAdd =
                match model.Selection with
                | English -> selectedEntry.English
                | Dutch -> selectedEntry.Dutch
            let textToAdd, remaining =
                match textToAdd with
                | [] -> [], []
                | s :: rest -> [s], rest
            let mergedEntry =
                match model.Selection with
                | English ->
                    { previousEntry with English = previousEntry.English @ textToAdd }
                | Dutch ->
                    { previousEntry with Dutch = previousEntry.Dutch @ textToAdd }
            let selectedEntry =
                match model.Selection with
                | English ->
                    { selectedEntry with English = remaining }
                | Dutch ->
                    { selectedEntry with Dutch = remaining }
            let tail =
                match remaining with
                | [] ->
                    // todo will it throw for the last entry?
                    let list = selectedEntry :: model.Entries |> List.skip (model.SelectedIndex + 1)
                    list.pairwise().map (fun (curr, next) ->
                        match model.Selection with
                        | English ->
                            { curr with English = next.English }
                        | Dutch ->
                            { curr with Dutch = next.Dutch })
                | _ -> selectedEntry :: model.Entries |> List.skip (model.SelectedIndex + 1)
            let entries = [
                yield! model.Entries
                |> List.take (model.SelectedIndex - 1)
                yield mergedEntry
                yield! tail
                // yield selectedEntry
                // // todo will it throw for the last entry?
                // yield! model.Entries |> List.skip (model.SelectedIndex + 1)
            ]
            let model = { model with Entries = entries }
            model, Cmd.none