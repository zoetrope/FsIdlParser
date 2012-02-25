module idl.preprocessor

open System
open System.Text


let deleteBlockComment (s : string) =
    let sb = new StringBuilder()
    let mutable inBlock = false
    let mutable skip = false

    let len = s.Length
    for i = 0 to len - 1 do
        
        if skip then
            skip <- false
        elif inBlock then
            if (s.[i] = '*') && (i < len-1) && (s.[i+1] = '/') then
                inBlock <- false
                skip <- true
                sb.Append("\n") |> ignore
        else
            if (s.[i] = '/') && (i < len-1) && (s.[i+1] = '*') then
                inBlock <- true
                skip <- true
            else
                sb.Append(s.[i]) |> ignore
    done
    sb.ToString()


let deleteLineComment (s : string) =
    let strs = s.Split('\n')
    let sb = new StringBuilder()

    for str in strs do
        let index = str.IndexOf("//")
        if index = -1 then
            sb.Append(str) |> ignore
        else
            let temp = str.Substring(0,index)
            sb.Append(temp) |> ignore
        sb.Append("\n") |> ignore
        
    done
    sb.ToString()

let deleteComment (s : string) : string =
    s |> deleteBlockComment
    |> deleteLineComment

    
let deleteDirective(s : string) =
    let strs = s.Split('\n')
    let sb = new StringBuilder()

    for str in strs do
        if not (str.StartsWith("#")) then
            sb.Append(str) |> ignore
        sb.Append("\n") |> ignore
    done
    sb.ToString()