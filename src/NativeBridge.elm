module NativeBridge exposing (..)

import Dict
import Task exposing (Task)
import NativeBridge.Types exposing (..)
import NativeBridge.Generation.Base exposing (..)


type alias RunGenerator =
    Program Never () ()


run : BridgeGenerator -> RunGenerator
run generator =
    Platform.program
        { init =
            ( ()
            , Task.attempt
                (\_ -> ())
                (writeFiles (generateFiles generator))
            )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


generator : String -> String -> BridgeGenerator
generator moduleName subject =
    { moduleName = moduleName
    , repoName = "binary-koan/elm-native-bridge"
    , subject = subject
    , types = []
    , functions = []
    , rawElm = ""
    , rawJs = ""
    , toJsConverters = Dict.empty
    , toElmConverters = Dict.empty
    }


withTypes : List TypeDefinition -> BridgeGenerator -> BridgeGenerator
withTypes types generator =
    { generator | types = generator.types ++ types }


withFunctions : List FunctionDefinition -> BridgeGenerator -> BridgeGenerator
withFunctions functions generator =
    { generator | functions = generator.functions ++ functions }


withElm : String -> BridgeGenerator -> BridgeGenerator
withElm code generator =
    { generator | rawElm = generator.rawElm ++ "\n" ++ code }


withJs : String -> BridgeGenerator -> BridgeGenerator
withJs code generator =
    { generator | rawJs = generator.rawJs ++ "\n" ++ code }



-- Working with types


record : String -> List String -> TypeDefinition
record name fields =
    Record { name = name, fields = fields }


union : String -> List ( String, String ) -> TypeDefinition
union name options =
    Union { name = name, options = options }


js : String -> JsValue
js str =
    RawJs str


jsString : String -> JsValue
jsString str =
    JsString str


toJs : String -> (String -> String) -> BridgeGenerator -> BridgeGenerator
toJs signature converter generator =
    { generator | toJsConverters = Dict.insert signature converter generator.toJsConverters }


toElm : String -> (String -> String) -> BridgeGenerator -> BridgeGenerator
toElm signature converter generator =
    { generator | toElmConverters = Dict.insert signature converter generator.toElmConverters }



-- Working with functions


function : String -> FunctionDefinition
function signature =
    { signature = signature, jsName = Nothing, isMethod = False }


jsFunction : String -> FunctionDefinition -> FunctionDefinition
jsFunction name fn =
    { fn | jsName = Just name }


isMethod : Bool -> FunctionDefinition -> FunctionDefinition
isMethod value fn =
    { fn | isMethod = value }



-- Type conversion


combineList : String -> (String -> String)
combineList joiner =
    \varName -> ""



-- TODO move to a Generation module


writeFiles : Result String GeneratedFiles -> Task String ()
writeFiles result =
    case result of
        Ok files ->
            (always (Task.succeed ())) (Debug.log (files.elmFile ++ "\n\n---\n\n" ++ files.jsFile) "")

        Err err ->
            Task.fail (Debug.log "Error" err)
