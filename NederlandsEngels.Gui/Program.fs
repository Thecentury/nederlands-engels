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
open NederlandsEngels.Gui.Model

(*--------------------------------------------------------------------------------------------------------------------*)

type MainWindow() as this =
  inherit HostWindow()

  do
    base.Title <- "Nederlands-Engels"
    base.WindowState <- WindowState.Maximized

    let root =
      if Directory.Exists "../Data" then
        // When running using 'dotnet run'
        "../Data"
      else
        // When running from the 'bin' folder
        "../../../../Data"

    // let en = loadHtml (Path.Combine (root, "en/1.xhtml")) |> parseEn
    // let nl = loadHtml (Path.Combine (root, "nl/1.xhtml")) |> parseNl
    //
    // let enSentences = en |> String.concat " " |> SentenceParsing.splitIntoSentences |> Array.toList
    // let nlSentences = nl |> String.concat " " |> SentenceParsing.splitIntoSentences |> Array.toList
    let enSentences = File.ReadAllLines (Path.Combine(root, "en/1.txt")) |> Array.toList
    let nlSentences = File.ReadAllLines (Path.Combine(root, "nl/1.txt")) |> Array.toList

    let emptyDisposable =
      { new IDisposable with
          member _.Dispose() = () }

    Elmish.Program.mkProgram (fun _ -> init enSentences nlSentences) update View.view
    |> Program.withHost this
    |> Program.withSubscription (fun _ -> [(["onKeyDown"], fun dispatch ->
        this.KeyDown.Add(fun e ->
          match e.Key, e.KeyModifiers with
          | Key.Left, KeyModifiers.None -> dispatch Msg.MoveFocusLeft
          | Key.Right, KeyModifiers.None -> dispatch Msg.MoveFocusRight
          | Key.Up, KeyModifiers.None -> dispatch Msg.MoveFocusUp
          | Key.Down, KeyModifiers.None -> dispatch Msg.MoveFocusDown
          | Key.M, KeyModifiers.None -> dispatch Msg.MergeUp
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
      // mainWindow.Content <- View.view mainWindow desktopLifetime
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
