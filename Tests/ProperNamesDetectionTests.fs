module Tests.ProperNamesDetectionTests

open System
open Xunit
open FsCheck.Xunit
open Swensen.Unquote

open NederlandsEngels.ProperNamesDetection

(*--------------------------------------------------------------------------------------------------------------------*)

[<Fact>]
let ``Detects proper names 1`` () =
  let actual = detectNames "Hello, Mr. Watson. I'm Sherlock Holmes. \"He is\". He said \"He\", 'He'. I'm from London. I'm a detective. And now I am going."
  let expected = [
    { Value = "Hello, Mr. "; Category = RegularText }
    { Value = "Watson"; Category = ProperName }
    { Value = ". I'm "; Category = RegularText }
    { Value = "Sherlock"; Category = ProperName }
    { Value = " "; Category = RegularText }
    { Value = "Holmes"; Category = ProperName }
    { Value = ". \"He is\". He said \""; Category = RegularText }
    { Value = "He"; Category = ProperName }
    { Value = "\", '"; Category = RegularText }
    { Value = "He"; Category = ProperName }
    { Value = "'. I'm from "; Category = RegularText }
    { Value = "London"; Category = ProperName }
    { Value = ". I'm a detective. And now I am going."; Category = RegularText }
  ]

  test <@ actual = expected @>

[<Fact>]
let ``Detects proper names 2`` () =
  let actual = detectNames "I——\"MY DEAR MR.\""
  let expected = [
    { Value = "I——\""; Category = RegularText }
    { Value = "MY"; Category = ProperName }
    { Value = " "; Category = RegularText }
    { Value = "DEAR"; Category = ProperName }
    { Value = " MR.\""; Category = RegularText }
  ]

  test <@ actual = expected @>

[<Property>]
let ``The length is not lost`` (s : string) =
  if String.IsNullOrEmpty s then true
  else
    let withNames = detectNames s
    let annotatedLength = withNames |> Seq.sumBy (fun n -> n.Value.Length)
    annotatedLength = s.Length