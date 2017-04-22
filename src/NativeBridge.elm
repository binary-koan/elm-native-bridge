module NativeBridge exposing (..)

import Task exposing (Task)
import NativeBridge.Fields exposing (..)
import NativeBridge.Params exposing (..)
import NativeBridge.Generator exposing (..)


type alias BridgeGenerator =
    Program Never () ()


generate : String -> String -> List BridgeType -> List BridgeFunction -> BridgeGenerator
generate moduleName subject types functions =
    Platform.program
        { init =
            ( ()
            , Task.perform
                (\_ -> ())
                (generateFiles moduleName subject types functions
                    |> Task.andThen writeFiles
                    |> Task.onError logError
                )
            )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


logError : String -> Task Never ()
logError error =
    (always (Task.succeed ())) (Debug.log "Error" error)


writeFiles : GeneratedFiles -> Task String ()
writeFiles files =
    (always (Task.succeed ())) (Debug.log (files.elmFile ++ "\n\n---\n\n" ++ files.jsFile) "")



-- Working with types


recordType : String -> List Field -> BridgeType
recordType name fields =
    RecordType { name = name, fields = fields }


unionType : String -> List ( String, BridgeType ) -> BridgeType
unionType name options =
    UnionType { name = name, options = options }



-- Working with functions


function : String -> List String -> BridgeFunction
function name definition =
    { name = name, elmName = name, definition = definition, prependParams = [], appendParams = [] }


prependParams : List FixedParam -> BridgeFunction -> BridgeFunction
prependParams params fn =
    { fn | prependParams = params ++ fn.prependParams }


appendParams : List FixedParam -> BridgeFunction -> BridgeFunction
appendParams params fn =
    { fn | appendParams = fn.appendParams ++ params }