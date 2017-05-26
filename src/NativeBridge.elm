module NativeBridge exposing (..)

import Task exposing (Task)
import NativeBridge.Fields exposing (..)
import NativeBridge.Params exposing (..)
import NativeBridge.Generator exposing (..)


type alias BridgeGenerator =
    Program Never () ()


generate : String -> String -> List BridgeEntity -> BridgeGenerator
generate moduleName subject types =
    Platform.program
        { init =
            ( ()
            , Task.perform
                (\_ -> ())
                (generateFiles moduleName subject types
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


record : String -> List Field -> BridgeEntity
record name fields =
    Record { name = name, fields = fields }


union : String -> List ( String, BridgeEntity ) -> BridgeEntity
union name options =
    Union { name = name, options = options }



-- Working with functions


function : String -> List Param -> FieldType -> BridgeEntity
function name params result =
    Function { name = name, elmName = name, params = params, result = result }
