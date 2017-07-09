module Types.Refine exposing (..)


type Type
    = BasicType
    | MaybeType Type
    | ListType Type
    | UnionType (List Type)
    | RecordType (List ( String, Type ))
