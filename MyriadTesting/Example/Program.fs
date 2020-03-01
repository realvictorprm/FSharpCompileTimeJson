// Learn more about F# at http://fsharp.org

open System
open Model

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    { foo = 1
      bar = ""
      spam = 2. }
    |> Test.Serializer.Serialize
    |> printfn "%A"
    Console.ReadKey() |> ignore
    0 // return an integer exit code
