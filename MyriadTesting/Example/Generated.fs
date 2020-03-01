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

    static member inline public Serialize(theObject : Model.SomeRecord) =
        JsonValue.Record([| "foo", Serializer.Serialize(theObject.foo)
                            "bar", Serializer.Serialize(theObject.bar)
                            "spam", Serializer.Serialize(theObject.spam) |])

    static member inline public Serialize(theObject : Model.SomeRecord2) =
        JsonValue.Record([| "foo", Serializer.Serialize(theObject.foo) |])
    static member inline public Serialize(theObject : Model.SomeRecord3) =
        JsonValue.Record([| "foo", Serializer.Serialize(theObject.foo)
                            "bar", Serializer.Serialize(theObject.bar) |])
