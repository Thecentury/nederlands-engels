module Program

open System.IO
open System.Xml
open System.Xml.Linq

(*--------------------------------------------------------------------------------------------------------------------*)

type MyResolver () =
    inherit XmlResolver ()

    override this.GetEntity (absoluteUri, role, ofObjectToReturn) =
        null

let loadXml (path : string) =
    use fs = new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read)
    let settings = XmlReaderSettings()
    settings.DtdProcessing <- DtdProcessing.Parse
    settings.XmlResolver <- MyResolver()
    // let xmlSchema = XmlSchema()
    // settings.Schemas.Add(xmlSchema) |> ignore
    use xmlReader = XmlReader.Create(fs, settings)
    // xmlReader.ResolveEntity ()
    XDocument.Load(xmlReader)

[<EntryPoint>]
let main _ =
    let en = loadXml @"C:\_temp\epub-dutch\en\1.xhtml"
    let nl = loadXml @"C:\_temp\epub-dutch\nl\1.xhtml"
    0
