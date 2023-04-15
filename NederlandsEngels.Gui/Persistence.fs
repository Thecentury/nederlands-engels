namespace NederlandsEngels.Gui

open System.IO
open Newtonsoft.Json

open NederlandsEngels
open NederlandsEngels.GUI.Model

open ProperNamesDetection

(*--------------------------------------------------------------------------------------------------------------------*)

type PersistenceEntry = {
    English : List<string>
    Dutch : List<string>
}

type State = List<PersistenceEntry>

module Persistence =

    let private toPersistenceEntry (entry : Entry) =
        let text (annotated : AnnotatedValue<_>) = annotated.Value
        let toString = List.map (List.map text >> String.concat "")
        { English = toString entry.English
          Dutch = toString entry.Dutch }

    let private toEntry (entry : PersistenceEntry) : Entry =
        let detectNames = List.map detectNames
        { English = detectNames entry.English
          Dutch = detectNames entry.Dutch }

    let save (fileName : string) (model : Model) =
        let state = model.Position |> Zipper.toList |> List.map toPersistenceEntry
        let json = JsonConvert.SerializeObject(state, Formatting.Indented)
        File.WriteAllText(fileName, json)

    let load (fileName : string) =
        let json = File.ReadAllText(fileName)
        let state = JsonConvert.DeserializeObject<State>(json)
        state |> List.map toEntry


