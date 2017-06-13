module NativeBridge.Types exposing (..)

import Dict exposing (Dict)
import Task exposing (Task)


type alias BridgeGenerator =
    { moduleName : String
    , repoName : String
    , subject : String
    , types : List TypeDefinition
    , functions : List FunctionDefinition
    , rawElm : String
    , rawJs : String
    , toJsConverters : Dict String (String -> String)
    , toElmConverters : Dict String (String -> String)
    }


type TypeDefinition
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


type alias FunctionDefinition =
    { jsName : Maybe String
    , signature : String
    , isMethod : Bool
    }


type JsValue
    = RawJs String
    | JsString String


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
