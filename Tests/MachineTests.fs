module Tests.MachineTests

open NederlandsEngels
open NederlandsEngels.Machines
open Xunit
open Swensen.Unquote

#nowarn "40"

let rec m = Machine.machine ^ fun (i : int) -> Transition (i, m)

let accumulateResults m inputs =
    let rec go m inputs soFar =
        match inputs with
        | [] -> soFar
        | i :: rest ->
            match Machine.step i m with
            | Halt -> soFar
            | Transition (o, m) -> go m rest (o :: soFar)
    go m inputs []

// [<Fact>]
// let prependInput () =
//     let m = SentenceParsing.prependInput m 1
//     let results = accumulateResults m [2]
//     
//     test <@ results = [1; 2] @>
