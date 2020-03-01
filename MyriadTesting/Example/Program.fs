// Learn more about F# at http://fsharp.org

open System
open Model

open JsonPlugin.Types

let rec jsonToString json =
    match json with
    | JsonValue.Record fields ->
        seq {
            for (fieldName, fieldValue) in fields -> sprintf "\"%s\":%s" fieldName (jsonToString fieldValue)
        } |> String.concat ","
        |> sprintf "{%s}"
    | JsonValue.Number number -> sprintf "%f" number 
    | JsonValue.String str -> sprintf "%A" str
    | JsonValue.Null -> "null"
    | JsonValue.Boolean b -> sprintf "%A" b
    | JsonValue.Float f -> sprintf "%f" f

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    { foo = 1
      bar = ""
      spam = 2. }
    |> Test.Serializer.Serialize
    |> jsonToString
    |> printfn "%s"
    SomeUnion.CaseA 10
    |> Test.Serializer.Serialize
    |> jsonToString
    |> printfn "%s"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
