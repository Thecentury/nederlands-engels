module Tests.TextTests

open NederlandsEngels
open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData("dear mr")>]
[<InlineData("dear Mr")>]
[<InlineData("hello, rev")>]
[<InlineData("dr")>]
let isAbbreviation (s : string) =
  let chars = s |> Seq.rev |> Seq.toList
  test <@ Text.isAbbreviation chars @>

[<Theory>]
[<InlineData("amr")>]
[<InlineData("xx")>]
[<InlineData(".dr")>]
let isNotAbbreviation (s : string) =
  let chars = s |> Seq.rev |> Seq.toList
  test <@ Text.isAbbreviation chars = false @>