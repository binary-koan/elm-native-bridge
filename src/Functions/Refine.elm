module Functions.Refine exposing (..)

import Types.Refine exposing (..)


type alias Function =
    { name : String
    , params : List Type
    , result : Type
    }
