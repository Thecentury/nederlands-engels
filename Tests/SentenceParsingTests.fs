namespace Tests

open Xunit
open Swensen.Unquote
open NederlandsEngels

// 'O, dat zit wel goed, ' riep hij vrolijk, 'ik denk dat we er vanuit kunnen gaan dat de zaak beklonken is... tenminste, als de kamers u bevallen. '
// 'O, het is dus geheim?' riep ik uit, en ik wreef in mijn handen.
// 'Tussen haakjes, ' vroeg ik plotseling.
// 'O, het is dus geheim?' riep ik uit.

type SentenceParsingTests () =
    
    static member example str expected =
        [| box str; box expected |]

    static member examples =
        [|
            SentenceParsingTests.example "'O, dat zit wel goed, ' riep hij vrolijk, 'ik denk dat we er vanuit kunnen gaan dat de zaak beklonken is... tenminste, als de kamers u bevallen. ' Hallo." [|
                "'O, dat zit wel goed, ' riep hij vrolijk, 'ik denk dat we er vanuit kunnen gaan dat de zaak beklonken is... tenminste, als de kamers u bevallen. '"
                "Hallo."
            |]
            SentenceParsingTests.example "'O, ' x 'y... z. ' H." [|
                "'O, ' x 'y... z. '"
                "H."
            |]
            SentenceParsingTests.example "'A?' b." [| "'A?' b." |]
            SentenceParsingTests.example "H. I." [|
                "H."
                "I."
            |]
            SentenceParsingTests.example "H.  I." [|
                "H."
                "I."
            |]
            SentenceParsingTests.example "Mr. Crab" [| "Mr. Crab" |]
        |]

    [<Theory>]
    [<MemberData("examples")>]
    member _.``Finds sentences`` (str : string, expected : array<string>) =
        let actual = SentenceParsing.splitIntoSentences str
        test <@ actual.Length = expected.Length @>
        if expected.Length = 1 then
            let actual = actual[0]
            let expected = expected[0]
            test <@ actual = expected @>
        else
            test <@ actual = expected @>