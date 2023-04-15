module NederlandsEngels.SeqMachines

open FSharp.Core.Fluent
open NederlandsEngels.Machines

module M = Machine

(*--------------------------------------------------------------------------------------------------------------------*)

type Input<'a> =
  | Element of 'a
  | EndOfInput

let applyToCharacters (s : string) m =
    let inputs = s.map(Element).append([EndOfInput])
    inputs
    |> Seq.mapFold (fun (machine, index) char ->
        let trans = M.step char machine
        match trans with
        | Halt ->
            let str = $"{s.Substring(0, index)}{s.Substring(index)}"
            let pointer = String.replicate index " " + "^"
            let msg = $"'Unexpected end of sentence at position {index + 1} ('{char}'):\n{str}\n{pointer}"
            failwith msg
        | Transition (newSentences, nextMachine) ->
            newSentences, (nextMachine, index + 1)
      ) (m, 0)
    |> fst
    |> Seq.toList