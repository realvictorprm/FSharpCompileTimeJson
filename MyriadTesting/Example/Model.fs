module Model

[<JsonPlugin.CompileTimeJsonEncoding()>]
type SomeRecord =
    { foo: int
      bar: string
      spam: float }
      
[<JsonPlugin.CompileTimeJsonEncoding>]
type SomeRecord2 =
    { foo: SomeRecord }

[<JsonPlugin.CompileTimeJsonEncoding>]
type SomeRecord3 =
    { foo: SomeRecord
      bar: SomeRecord2 }
      
[<JsonPlugin.CompileTimeJsonEncoding>]
type SomeUnion =
    | CaseA of a: int
    | CaseB of value: SomeRecord
    | CaseC of b: bool
    //| CaseD of arr: bool array
    | CaseE