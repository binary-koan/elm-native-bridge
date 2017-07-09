module Functions.Refine exposing (..)

import Functions.Discover exposing (..)
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


refineFunctions : List DiscoveredFunction -> Result String (List Function)
refineFunctions fns =
    -- TODO
    Ok []
