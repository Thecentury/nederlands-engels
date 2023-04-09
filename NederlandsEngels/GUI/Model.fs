module NederlandsEngels.GUI.Model

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
}
with member this.IsEmpty = this.English.IsEmpty && this.Dutch.IsEmpty

let englishLens = Lens.create (fun e -> e.English) (fun e english -> { e with English = english })
let dutchLens = Lens.create (fun e -> e.Dutch) (fun e dutch -> { e with Dutch = dutch })

let sentencesLens selection =
    match selection with
    | English -> englishLens
    | Dutch -> dutchLens

let private mkEntry (en : Option<string>) (nl : Option<string>) =
    match en, nl with
    | None, None -> None
    | en, nl -> Some { English = Option.toList en; Dutch = Option.toList nl }

(*--------------------------------------------------------------------------------------------------------------------*)

type UndoRedoStack<'a> = {
    Undos : List<'a>
    Redos : List<'a>
}

module UndoRedo =

    let empty =
        { Undos = []
          Redos = [] }

    let push (stack : UndoRedoStack<'a>) (item : 'a) =
        { Undos = item :: stack.Undos
          Redos = [] }

    let pushNew (stack : UndoRedoStack<'a>) (item : 'a) =
        match stack.Undos with
        | [] -> push stack item
        | undo :: _ ->
            if undo = item then stack
            else push stack item

    let undo (stack : UndoRedoStack<'a>) =
        match stack.Undos with
        | [] -> None
        | undo :: otherUndos ->
            let stack = { Undos = otherUndos
                          Redos = undo :: stack.Redos }
            Some (stack, undo)

    let redo (stack : UndoRedoStack<'a>) =
        match stack.Redos with
        | [] -> None
        | redo :: otherRedos ->
            let stack = { Undos = redo :: stack.Undos
                          Redos = otherRedos }
            Some (stack, redo)

(*--------------------------------------------------------------------------------------------------------------------*)

type Position = Zipper<Entry>

// todo undo/redo
type Model = {
    Position : Position
    History : UndoRedoStack<Position>
    Selection : Selection
    ShowRowsBeforeAfter : int
}

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
        Position = Zipper.fromList entries
        History = UndoRedo.empty
        Selection = English
        ShowRowsBeforeAfter = 10
    }
    model, Cmd.none

let private positionLens = Lens.create (fun m -> m.Position) (fun m position -> { m with Position = position })

type Msg =
    | MoveFocusLeft
    | MoveFocusRight
    | MoveFocusUp
    | MoveFocusDown
    | MergeUp
    | IncreaseRowsBeforeAfter
    | DecreaseRowsBeforeAfter
    | Undo
    | Redo

let updateCore (msg : Msg) (model : Model) =
    match msg with
    | MoveFocusLeft -> { model with Selection = English }
    | MoveFocusRight -> { model with Selection = Dutch }
    | MoveFocusUp -> Lens.tryModify positionLens model Zipper.tryMoveLeft
    | MoveFocusDown -> Lens.tryModify positionLens model Zipper.tryMoveRight
    | IncreaseRowsBeforeAfter -> { model with ShowRowsBeforeAfter = model.ShowRowsBeforeAfter + 1 }
    | DecreaseRowsBeforeAfter -> { model with ShowRowsBeforeAfter = max 0 model.ShowRowsBeforeAfter - 1 }
    | Undo ->
        match UndoRedo.undo model.History with
        | None -> model
        | Some (history, position) -> { model with Position = position; History = history }
    | Redo ->
        match UndoRedo.redo model.History with
        | None -> model
        | Some (history, position) -> { model with Position = position; History = history }
    | MergeUp -> // todo tests
        let sentencesLens = sentencesLens model.Selection
        match model.Position.Left, sentencesLens.Get model.Position.Focus with
        | left :: otherLeft, firstSentence :: otherSentences ->
            let left = Lens.modify sentencesLens (fun sentences -> sentences @ [firstSentence]) left
            let focus, right =
                match otherSentences with
                | [] ->
                    match model.Position.Right with
                    | [] ->
                        let focus = sentencesLens.Set model.Position.Focus []
                        focus, []
                    | right :: _ as rights ->
                        let focus = sentencesLens.Set model.Position.Focus (sentencesLens.Get right)
                        let rights =
                            rights
                            |> List.withNext
                            |> List.choose (fun (curr, next) ->
                                let curr =
                                    match next with
                                    | None -> sentencesLens.Set curr []
                                    | Some next -> sentencesLens.Set curr (sentencesLens.Get next)
                                if curr.IsEmpty then None
                                else Some curr)
                        focus, rights
                | otherSentences ->
                    let focus = sentencesLens.Set model.Position.Focus otherSentences
                    focus, model.Position.Right
            let position = { Left = left :: otherLeft; Focus = focus; Right = right }
            let position =
                if focus.IsEmpty then
                    Zipper.tryMoveLeft position
                    |> Option.map (fun position -> { position with Right = [] })
                    |> Option.defaultValue position
                else
                    position
            { model with Position = position }
        | _, _ -> model

let update (msg : Msg) (model : Model) =
    match msg with
    | Undo | Redo -> updateCore msg model, Cmd.none
    | msg ->
        let model' = updateCore msg model
        let model' =
            if model'.Position <> model.Position then
                { model' with History = UndoRedo.push model.History model.Position }
            else
                model'
        model', Cmd.none