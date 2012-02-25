open System
open System.CodeDom
open System.IO

open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

open idl.ast
open idl.parser
open idl.preprocessor

[<EntryPoint>]
let main(argv: string[]) =

    //let text = File.ReadAllText("../../../sample/BasicDataType.idl")
    //let text = File.ReadAllText("../../../sample/DataPort.idl")
    //let text = File.ReadAllText("../../../sample/Manager.idl")
    //let text = File.ReadAllText("../../../sample/OpenRTM.idl")
    //let text = File.ReadAllText("../../../sample/RTC.idl")
    let text = File.ReadAllText("../../../sample/SDOPackage.idl")
                |> fun s -> s.Replace("\r\n", "\n")
                |> deleteComment
                |> deleteDirective
    
    //printfn "%A" text

    let test = (specification |> run) <| text
    match test with
        | Success(r, _, _) -> printfn "%A" r
        | Failure (msg, err, _) -> printfn "%s" msg


    0