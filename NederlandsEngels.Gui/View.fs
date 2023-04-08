module NederlandsEngels.Gui.View

open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Controls
open Avalonia.Media

open NederlandsEngels.Gui.Model

(*--------------------------------------------------------------------------------------------------------------------*)

let private renderEntry _dispatch index selection (entry : Entry) =
    let renderString expectedSelection s =
        let tb =
            TextBlock.create [
                match selection with
                | None -> ()
                | Some s when expectedSelection = s ->
                    TextBlock.init (fun (tb : TextBlock) ->
                        tb.Focus())
                | _ -> ()
                TextBlock.textWrapping TextWrapping.Wrap
                TextBlock.focusable true
                TextBlock.text s
            ] |> generalize

        Border.create [
            match selection with
            | Some s when expectedSelection = s ->
                Border.borderBrush Brushes.Gray
            | _ -> Border.borderBrush Brushes.Transparent
            Border.borderThickness 1.
            Border.child tb
        ] |> generalize

    [
        StackPanel.create [
            Grid.column 0
            Grid.row index
            StackPanel.margin (10, 2, 10, 2)
            StackPanel.children (entry.English |> List.map (renderString Selection.English))
        ] |> generalize
        StackPanel.create [
            Grid.column 1
            Grid.row index
            StackPanel.margin (10, 2, 10, 2)
            StackPanel.children (entry.Dutch |> List.map (renderString Selection.Dutch))
        ] |> generalize
    ]

let view (model : Model) dispatch =
    let rows = RowDefinitions()
    for _ in model.Entries do
        rows.Add (RowDefinition(Height = GridLength.Auto))

    Grid.create [
        Grid.children [
            ScrollViewer.create [
                ScrollViewer.content(
                    Grid.create [
                        Grid.columnDefinitions "*,*"
                        Grid.rowDefinitions rows
                        Grid.children (
                            model.Entries
                            |> Seq.mapi (fun i ->
                                let selection = if i = model.SelectedIndex then Some model.Selection else None
                                renderEntry dispatch i selection)
                            |> Seq.collect id
                            |> Seq.toList
                        )
                    ])
            ]
        ]
    ]