module NederlandsEngels.Gui.View

open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Controls

open Avalonia.Media
open NederlandsEngels
open NederlandsEngels.Gui.Model

(*--------------------------------------------------------------------------------------------------------------------*)

let private renderEntry dispatch i (entry : Entry) =
    let renderString s =
        TextBlock.create [
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.text s
        ] |> generalize

    [
        StackPanel.create [
            Control.init (fun s -> s.Background <- SolidColorBrush(Colors.LightGray))
            Grid.column 0
            Grid.row i
            StackPanel.margin (10, 2, 10, 2)
            StackPanel.children (entry.English |> List.map renderString)
        ] |> generalize
        StackPanel.create [
            Grid.column 1
            Grid.row i
            StackPanel.margin (10, 2, 10, 2)
            StackPanel.children (entry.Dutch |> List.map renderString)
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
                        Grid.children (model.Entries |> Seq.mapi (renderEntry dispatch) |> Seq.collect id |> Seq.toList)
                    ])
            ]
        ]
    ]