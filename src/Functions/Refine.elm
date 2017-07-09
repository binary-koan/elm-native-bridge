module Functions.Refine exposing (..)

import Types.Refine exposing (..)


type alias Function =
    { name : String
    , params : List Type
    , result : FunctionOutput
    }


type FunctionOutput
    = BasicOutput Type
    | ResultOutput Type Type
    | TaskOutput Type Type
