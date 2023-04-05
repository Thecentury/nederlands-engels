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
            SentenceParsingTests.example "H. I." [|
                "H."
                "I."
            |]
            SentenceParsingTests.example "H.  I." [|
                "H."
                "I."
            |]
        |]

    [<Theory>]
    [<MemberData("examples")>]
    member _.``Finds sentences`` (str : string, expected : array<string>) =
        let actual = SentenceParsing.findSentences str
        test <@ actual = expected @>