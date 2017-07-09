module Types.Refine exposing (..)


type Type
    = BasicType
    | MaybeType Type
    | ListType Type
    | DictType Type
    | UnionType UnionOptions
    | RecordType String (List ( String, Type ))


type alias UnionOptions =
    { name : String
    , values : List ( String, String )
    , defaultCtor : String
    , defaultJs : String
    }
