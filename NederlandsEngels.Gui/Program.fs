namespace NederlandsEngels.Gui

open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls.ApplicationLifetimes
open Elmish
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish

open NederlandsEngels
open NederlandsEngels.EpubParsing

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

    let en = loadHtml (Path.Combine (root, "en/1.xhtml")) |> parseEn
    let nl = loadHtml (Path.Combine (root, "nl/1.xhtml")) |> parseNl

    let enSentences = en |> String.concat " " |> SentenceParsing.splitIntoSentences |> Array.toList
    let nlSentences = nl |> String.concat " " |> SentenceParsing.splitIntoSentences |> Array.toList

    Elmish.Program.mkProgram (fun _ -> Model.init enSentences nlSentences) Model.update View.view
    |> Program.withHost this
    // |> Program.withConsoleTrace
    |> Program.run
  // base.Height <- 400.0
  // base.Width <- 400.0

  //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
  //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

type App() =
  inherit Application()

  override this.Initialize () =
    this.Styles.Add (FluentTheme (baseUri = null, Mode = FluentThemeMode.Light))

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
    let opts = AvaloniaNativePlatformOptions (UseGpu = false)
    AvaloniaLocator.CurrentMutable.BindToSelf opts |> ignore

    AppBuilder.Configure<App>().UsePlatformDetect().UseSkia().StartWithClassicDesktopLifetime args
