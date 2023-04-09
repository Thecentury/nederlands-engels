namespace NederlandsEngels.Gui

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Styling
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls.ApplicationLifetimes
open Elmish
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish

open NederlandsEngels
open NederlandsEngels.GUI.Model

(*--------------------------------------------------------------------------------------------------------------------*)

module FileSystem =

  let root =
    if Directory.Exists "../Data" then
      // When running using 'dotnet run'
      "../Data"
    else
      // When running from the 'bin' folder
      "../../../../Data"

type MainWindow() as this =
  inherit HostWindow()

  do
    base.Title <- "Nederlands-Engels"
    base.WindowState <- WindowState.Maximized

    let root = FileSystem.root

    let stateFile = Path.Combine(root, "mapping.json")
    let model =
      if File.Exists stateFile then
        let state = Persistence.load stateFile
        createFromState state
      else
        let enSentences = File.ReadAllLines (Path.Combine(root, "en/1.txt")) |> Array.toList
        let nlSentences = File.ReadAllLines (Path.Combine(root, "nl/1.txt")) |> Array.toList
        createFromSentences enSentences nlSentences

    let emptyDisposable =
      { new IDisposable with
          member _.Dispose() = () }

    Elmish.Program.mkProgram (fun _ -> model, Cmd.none) update ViewZipper.view
    |> Program.withHost this
    |> Program.withTermination ((=) Msg.Exit) (fun model ->
      Persistence.save stateFile model
      this.Close ())
    |> Program.withSubscription (fun _ -> [(["onKeyDown"], fun dispatch ->
        this.KeyDown.Add(fun e ->
          match e.Key, e.KeyModifiers with
          | Key.Left, KeyModifiers.None -> dispatch Msg.MoveFocusLeft
          | Key.Right, KeyModifiers.None -> dispatch Msg.MoveFocusRight
          | Key.Up, KeyModifiers.None -> dispatch Msg.MoveFocusUp
          | Key.Down, KeyModifiers.None -> dispatch Msg.MoveFocusDown
          | Key.M, KeyModifiers.None -> dispatch Msg.MergeUp
          | Key.OemPlus, KeyModifiers.None -> dispatch Msg.IncreaseRowsBeforeAfter
          | Key.OemMinus, KeyModifiers.None -> dispatch Msg.DecreaseRowsBeforeAfter
          | Key.U, KeyModifiers.None -> dispatch Msg.Undo
          | Key.R, KeyModifiers.None -> dispatch Msg.Redo
          | Key.Q, _ -> dispatch Msg.Exit
          | _ -> ())
        emptyDisposable
      )])
    // |> Program.withConsoleTrace
    |> Program.run
  // base.Height <- 400.0
  // base.Width <- 400.0

  //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
  //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

type App() =
  inherit Application()

  override this.Initialize () =
    this.Styles.Add (FluentTheme())
    this.RequestedThemeVariant <- ThemeVariant.Light

  override this.OnFrameworkInitializationCompleted () =
    match this.ApplicationLifetime with
    | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
      let mainWindow = MainWindow ()
      desktopLifetime.MainWindow <- mainWindow
      desktopLifetime.ShutdownMode <- ShutdownMode.OnMainWindowClose
    | _ -> ()

module Program =

  [<CompiledName "BuildAvaloniaApp">]
  let buildAvaloniaApp () =
    AppBuilder.Configure<App>().UsePlatformDetect().LogToTrace (areas = Array.empty)

  [<EntryPoint>]
  let main (args : string[]) =
    // let existingOptions = AvaloniaLocator.Current.GetService<AvaloniaNativePlatformOptions>()
    // let opts = AvaloniaNativePlatformOptions (UseGpu = false)
    // AvaloniaLocator.CurrentMutable.BindToSelf opts |> ignore
    AppBuilder.Configure<App>().UsePlatformDetect().UseSkia().StartWithClassicDesktopLifetime args
