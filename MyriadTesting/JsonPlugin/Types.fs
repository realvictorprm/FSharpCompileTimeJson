module JsonPlugin.Types

type JsonValue =
| String of string
| Number of decimal
| Float of float
| Record of properties:(string * JsonValue)[]
| Array of elements:JsonValue[]
| Boolean of bool
| Null