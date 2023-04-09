namespace NederlandsEngels.Gui

open System.IO
open NederlandsEngels
open NederlandsEngels.GUI.Model
open Newtonsoft.Json

(*--------------------------------------------------------------------------------------------------------------------*)

type PersistenceEntry = {
    English : List<string>
    Dutch : List<string>
}

type State = List<PersistenceEntry>

module Persistence =

    let private toPersistenceEntry (entry : Entry) =
        { English = entry.English
          Dutch = entry.Dutch }

    let private toEntry (entry : PersistenceEntry) : Entry =
        { English = entry.English
          Dutch = entry.Dutch }

    let save (fileName : string) (model : Model) =
        let state = model.Position |> Zipper.toList |> List.map toPersistenceEntry
        let json = JsonConvert.SerializeObject(state, Formatting.Indented)
        File.WriteAllText(fileName, json)

    let load (fileName : string) =
        let json = File.ReadAllText(fileName)
        let state = JsonConvert.DeserializeObject<State>(json)
        state |> List.map toEntry


