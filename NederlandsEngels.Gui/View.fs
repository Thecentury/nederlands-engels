module NederlandsEngels.Gui.View

open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Controls
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media

open NederlandsEngels.GUI.Model

(*--------------------------------------------------------------------------------------------------------------------*)

let private renderEntry _dispatch selection (entry : Entry) =
    let renderString s =
        TextBlock.create [
            TextBlock.textWrapping TextWrapping.Wrap
            TextBlock.text s
        ] |> generalize

    let wrapWithBorder column expectedSelection (element : IView) =
        Border.create [
            match selection with
            | Some s when expectedSelection = s ->
                Border.borderBrush Brushes.CornflowerBlue
            | _ -> Border.borderBrush Brushes.Transparent
            Border.borderThickness 1.
            Border.child element
            Grid.column column
            Border.margin (10, 2, 10, 2)
        ] |> generalize

    Grid.create [
        Grid.columnDefinitions "*,*"
        Grid.children [
            StackPanel.create [
                StackPanel.children (entry.English |> List.map renderString)
            ] |> wrapWithBorder 0 Selection.English
            StackPanel.create [
                StackPanel.children (entry.Dutch |> List.map renderString)
            ] |> wrapWithBorder 1 Selection.Dutch
        ]
    ] |> generalize

let view (model : Model) dispatch =
    let before =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            Grid.row 0
            StackPanel.verticalAlignment VerticalAlignment.Bottom
            StackPanel.children (
                model.Position.Left
                |> Seq.truncate model.ShowRowsBeforeAfter
                |> Seq.rev
                |> Seq.map (renderEntry dispatch None)
                |> Seq.toList
            )
        ] |> generalize
    let focus =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            Grid.row 1
            StackPanel.children [
                renderEntry dispatch (Some model.Selection) model.Position.Focus
            ]
        ] |> generalize
    let after =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            Grid.row 2
            StackPanel.verticalAlignment VerticalAlignment.Top
            StackPanel.children (
                model.Position.Right
                |> Seq.truncate model.ShowRowsBeforeAfter
                |> Seq.map (renderEntry dispatch None)
                |> Seq.toList
            )
        ] |> generalize
    let grid =
      Grid.create [
          Grid.rowDefinitions "*,Auto,*"
          Grid.children [
              before
              focus
              after
          ]
      ]
    DockPanel.create [
      DockPanel.lastChildFill true
      DockPanel.children [
          Grid.create [
            DockPanel.dock Dock.Top
            Grid.columnDefinitions "*,*"
            Grid.margin (0, 5)
            Grid.horizontalAlignment HorizontalAlignment.Stretch
            Grid.children [
                TextBlock.create [
                    TextBlock.text $"English: {model.EnglishPosition} / {model.EnglishSentences}"
                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                    Grid.column 0
                ] |> generalize
                TextBlock.create [
                    TextBlock.text $"Dutch: {model.DutchPosition} / {model.DutchSentences}"
                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                    Grid.column 1
                ] |> generalize
            ]
          ]
          grid
      ]
    ]