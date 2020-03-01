//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------
namespace rec Test


namespace Test

open JsonPlugin.Types

type Serializer =
    static member inline public Serialize(str : string) = JsonValue.String(str)
    static member inline public Serialize(number : int16) = JsonValue.Number(decimal (number))
    static member inline public Serialize(number : int32) = JsonValue.Number(decimal (number))
    static member inline public Serialize(number : int64) = JsonValue.Number(decimal (number))
    static member inline public Serialize(number : uint16) = JsonValue.Number(decimal (number))
    static member inline public Serialize(number : uint32) = JsonValue.Number(decimal (number))
    static member inline public Serialize(number : uint64) = JsonValue.Number(decimal (number))
    static member inline public Serialize(number : float) = JsonValue.Number(decimal (number))
    static member inline public Serialize(number : float32) = JsonValue.Number(decimal (number))
    static member inline public Serialize(number : byte) = JsonValue.Number(decimal (number))
    static member inline public Serialize(number : sbyte) = JsonValue.Number(decimal (number))

    static member inline public Serialize(somerecord : Model.SomeRecord) =
        JsonValue.Record([| "foo", Serializer.Serialize(somerecord.foo)
                            "bar", Serializer.Serialize(somerecord.bar)
                            "spam", Serializer.Serialize(somerecord.spam) |])

    static member inline public Serialize(somerecord2 : Model.SomeRecord2) =
        JsonValue.Record([| "foo", Serializer.Serialize(somerecord2.foo) |])

    static member inline public Serialize(somerecord3 : Model.SomeRecord3) =
        JsonValue.Record([| "foo", Serializer.Serialize(somerecord3.foo)
                            "bar", Serializer.Serialize(somerecord3.bar) |])

    static member inline public Serialize(someunion : Model.SomeUnion) =
        JsonValue.Record [| match someunion with
                            | Model.SomeUnion.CaseA a -> "CaseA", JsonValue.Record([| "a", Serializer.Serialize(a) |])
                            | Model.SomeUnion.CaseB value ->
                                "CaseB", JsonValue.Record([| "value", Serializer.Serialize(value) |])
                            | Model.SomeUnion.CaseC _ -> "CaseC", JsonValue.Record([||]) |]
