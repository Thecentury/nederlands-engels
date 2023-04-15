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
open NederlandsEngels.EpubParsing
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

module Args =

  let parseArgs (args : array<string>) =
    match args |> List.ofArray with
    | [] -> "1"
    | [chapter] -> chapter
    | _ -> failwith "Too many arguments"

type MainWindow(chapterName : string) as this =
  inherit HostWindow()

  do
    base.Title <- $"Nederlands-Engels, chapter {chapterName}"
    base.WindowState <- WindowState.Maximized

    let root = FileSystem.root

    let stateFile = Path.Combine(root, $"mapping-{chapterName}.json")
    let model =
      if File.Exists stateFile then
        let state = Persistence.load stateFile
        createFromState state
      else
        let en = loadHtml (Path.Combine (root, $"en/{chapterName}.xhtml")) |> parseEn
        let nl = loadHtml (Path.Combine (root, $"nl/{chapterName}.xhtml")) |> parseNl

        let enSentences = en |> String.concat " " |> SentenceParsing.splitIntoSentences |> Array.toList
        let nlSentences = nl |> Seq.collect SentenceParsing.splitIntoSentences |> Seq.toList

        createFromSentences enSentences nlSentences

    Elmish.Program.mkProgram (fun _ -> model, Cmd.none) update View.view
    |> Program.withHost this
    |> Program.withTermination ((=) Msg.Exit) (fun model ->
      Persistence.save stateFile model
      Environment.Exit 0
      )
    |> Program.withSubscription (fun _ -> [(["onKeyDown"], fun dispatch ->
        let onKeyDown =
          fun (e : KeyEventArgs) ->
              let dispatch msg =
                dispatch msg
                e.Handled <- true
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
              | Key.Escape, KeyModifiers.None
              | Key.Q, _ -> dispatch Msg.Exit
              | _ -> ()
        let onKeyDown = EventHandler<KeyEventArgs>(fun _ -> onKeyDown)
        this.KeyDown.AddHandler(onKeyDown)
        Disposable.ofFunction (fun () -> this.KeyDown.RemoveHandler(onKeyDown))
      )])
    // |> Program.withConsoleTrace
    |> Program.withErrorHandler (fun (msg, e) -> printfn $"Error msg: %s{msg}, error: %O{e}")
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
      let mainWindow = MainWindow (Args.parseArgs desktopLifetime.Args)
      desktopLifetime.MainWindow <- mainWindow
      desktopLifetime.ShutdownMode <- ShutdownMode.OnMainWindowClose
    | _ -> ()

module Program =

  [<CompiledName "BuildAvaloniaApp">]
  let buildAvaloniaApp () =
    AppBuilder.Configure<App>().UsePlatformDetect().LogToTrace (areas = Array.empty)

  [<EntryPoint>]
  let main (args : string[]) =
    AppDomain.CurrentDomain.UnhandledException.Add(fun e ->
      printfn $"Unhandled exception: %A{e.ExceptionObject}")
    try
    // let existingOptions = AvaloniaLocator.Current.GetService<AvaloniaNativePlatformOptions>()
    // let opts = AvaloniaNativePlatformOptions (UseGpu = false)
    // AvaloniaLocator.CurrentMutable.BindToSelf opts |> ignore
      let exitCode = AppBuilder.Configure<App>().UsePlatformDetect().UseSkia().StartWithClassicDesktopLifetime args
      exitCode
    with
    e ->
      printfn $"Exception: %A{e}"
      1
