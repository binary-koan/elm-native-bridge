module Types.Refine exposing (..)


type Type
    = BasicType
    | MaybeType Type
    | ListType Type
    | UnionType String (List String)
    | RecordType String (List ( String, Type ))
