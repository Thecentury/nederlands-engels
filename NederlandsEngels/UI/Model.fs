module NederlandsEngels.UI.Model

open FSharp.Core.Fluent
open Terminal.Gui
open Terminal.Gui.Elmish

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

let init (en : List<string>) (nl : List<string>) : Model * Cmd<Msg> =
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

let update (msg : Msg) (model : Model) =
    match msg with
    | () ->
        model, Cmd.none

let private renderEntry (entry : Entry) (_dispatch : Msg -> unit) =
    View.frameView [
        prop.height.filled
        prop.width.filled
        frameView.children [
            View.window [
                prop.width.percent 50
                prop.height.sized 2
                window.children [
                    for en in entry.English do
                        yield View.textView [
                            prop.position.x.at 0
                            prop.position.y.at 0
                            prop.width.filled
                            // prop.height.filled
                            prop.height.sized 2
                            prop.color (Color.BrightMagenta, Color.Blue)
                            textView.text en
                        ]
                ]
            ]
            View.window [
                prop.position.x.percent 50
                prop.width.percent 50
                prop.height.sized 2
                window.children [
                    for en in entry.English do
                        yield View.textView [
                            prop.position.x.at 0
                            prop.position.y.at 0
                            prop.width.filled
                            prop.height.sized 2
                            prop.color (Color.BrightMagenta, Color.Blue)
                            textView.text en
                        ]
                ]
            ]
        ]
    ]

let private localView (model : Model) (dispatch : Msg -> unit) =
    View.frameView [
        prop.position.x.at 0
        prop.position.y.at 0
        prop.width.fill 0
        prop.height.fill 0
        frameView.children [

            View.scrollView [
                prop.position.x.at 0
                prop.position.y.at 0
                prop.width.filled
                prop.height.filled
                scrollView.contentSize (Size(120, 120))
                scrollView.showHorizontalScrollIndicator false
                scrollView.showVerticalScrollIndicator true
                scrollView.children [
                    for entry in model.Entries do
                        yield renderEntry entry dispatch
                    // View.label [
                    //     prop.position.x.at 0
                    //     prop.position.y.at 0
                    //     prop.width.sized 120
                    //     prop.height.sized 120
                    //     prop.color (Color.BrightMagenta, Color.Blue)
                    //     label.text "Hello"
                    // ]
                ]
            ]
        ]
    ]

let globalView (model : Model) (dispatch : Msg -> unit) =
    View.page [
        prop.onKeyDown (fun key ->
            match key.KeyEvent.Key with
            | Key.Esc
            | Key.F10 -> Application.RequestStop ()
            | _ -> ()
        )
        // View.window [
        //     // prop.colorScheme Colors.Error
        //     prop.position.x.at 0
        //     prop.position.y.at 0
        //     prop.width.filled
        //     prop.height.filled
        //     // window.title $"Elmish Console Demo - {model.CurrentLocalTime:``yyyy-MM-dd HH:mm:ss.ms``}"
        //     window.children [
        //         yield! localView model dispatch
        //     ]
        // ]
        page.children [
            View.frameView [
                prop.position.x.at 0
                prop.position.y.at 0
                prop.width.filled
                prop.height.filled
                frameView.children [
                    localView model dispatch
                ]
            ]
        ]
    ]

let run (en : List<string>) (nl : List<string>) =
    Program.mkProgram (fun () -> init en nl) update globalView
    |> Program.run