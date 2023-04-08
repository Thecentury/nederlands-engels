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
            SentenceParsingTests.example "Boy's band" [| "Boy's band" |]
            SentenceParsingTests.example "\"A. B.\"" [| "\"A. B.\"" |]
            SentenceParsingTests.example "'A, ' b. C." [| "'A, ' b."; "C." |]
            SentenceParsingTests.example "\"A?\" b." [| "\"A?\" b." |]
            SentenceParsingTests.example "\"Q?\" I asked." [| "\"Q?\" I asked." |]
            SentenceParsingTests.example "\"He. If.\"" [| "\"He. If.\"" |]
            SentenceParsingTests.example "\"A,\" h. \"B.\"" [| "\"A,\" h."; "\"B.\"" |]
            SentenceParsingTests.example
                    "\"He either avoids the place for weeks, or else he works there from morning to night. If you like, we shall drive round together after luncheon.\""
                    [| "\"He either avoids the place for weeks, or else he works there from morning to night. If you like, we shall drive round together after luncheon.\"" |]
            SentenceParsingTests.example
                "\"Poor devil!\" he said, commiseratingly, after he had listened to my misfortunes. \"What are you up to now?\""
                [| "\"Poor devil!\" he said, commiseratingly, after he had listened to my misfortunes."; "\"What are you up to now?\"" |]
            SentenceParsingTests.example
                "\"P!\" s. \"W?\""
                [| "\"P!\" s."; "\"W?\"" |]
            SentenceParsingTests.example
                "\"What are you up to now?\""
                [| "\"What are you up to now?\"" |]
            SentenceParsingTests.example
                "\"Looking for lodgings.\" I answered."
                [| "\"Looking for lodgings.\" I answered." |]
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