// Learn more about F# at http://fsharp.org

open System
open Model

(*
static member inline Encode(list : #array< ^c>) =
    let inline call2 (a: ^a, b: ^b) = ((^a or ^b or ^c) : (static member Encode: ^b -> JsonValue)(b))
    [| for element in list -> call2 (Unchecked.defaultof<Encoder>, element) |]
    |> JsonValue.Array
*)

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
    //| JsonValue.Array arr -> ""

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    { foo = 1
      bar = ""
      spam = 2. }
    |> Test.Encoder.Encode
    |> jsonToString
    |> printfn "%s"
    SomeUnion.CaseA 10
    |> Test.Encoder.Encode
    |> jsonToString
    |> printfn "%s"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
