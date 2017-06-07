module NativeBridge.Types exposing (..)

import Dict exposing (Dict)
import Task exposing (Task)


type alias BridgeGenerator =
    { moduleName : String
    , subject : String
    , types : List BridgeType
    , functions : List BridgeFunction
    , rawElm : String
    , rawJS : String
    , valueConverters : Dict String Converter
    , resultTransformers : Dict String Transformer
    }


type BridgeType
    = Record RecordOptions
    | Union UnionOptions


type alias RecordOptions =
    { name : String
    , fields : List String
    }


type alias UnionOptions =
    --TODO paramteterised unions?
    { name : String
    , options : List ( String, String )
    }


type alias BridgeFunction =
    { jsName : String
    , elmName : String
    , definition : String
    }


type alias Converter =
    {}


type Transformer
    = UndefinedTransformer
    | DefinedTransformer (TransformerOptions -> BridgeGenerator -> Task String String)


type alias TransformerOptions =
    { typeName : String
    , typeParams : List String
    }


type alias TransformerResult =
    { returnValueWrapper : String -> String
    , extraParams : String
    }
