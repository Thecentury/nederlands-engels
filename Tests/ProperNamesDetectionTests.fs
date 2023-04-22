module Tests.ProperNamesDetectionTests

open Xunit
open Swensen.Unquote

open NederlandsEngels.ProperNamesDetection

(*--------------------------------------------------------------------------------------------------------------------*)

[<Fact>]
let ``Detects proper names`` () =
  let withNames = detectNames "Hello, Watson. I'm Sherlock Holmes. I'm from London. I'm a detective. And now I am going."
  let expected = [
    { Value = "Hello, "; Category = RegularText }
    { Value = "Watson"; Category = ProperName }
    { Value = ". I'm "; Category = RegularText }
    { Value = "Sherlock"; Category = ProperName }
    { Value = " "; Category = RegularText }
    { Value = "Holmes"; Category = ProperName }
    { Value = ". I'm from "; Category = RegularText }
    { Value = "London"; Category = ProperName }
    { Value = ". I'm a detective. And now I am going."; Category = RegularText }
  ]

  test <@ withNames = expected @>