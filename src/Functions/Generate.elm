module Functions.Generate exposing (..)

import Utils.Interpolate exposing (..)
import Functions.Refine exposing (..)


generateFunction : Function -> String
generateFunction fn =
    interpolate """
        functions.{0} = {1}((...args) => {
            args = [{2}]
            return {3}
        })
        """
        [ fn.name, wrapperName fn, generateArgs fn, wrapResult fn ]


generateArgs : Function -> String
generateArgs fn =
    let
        convertArg i t =
            convertType ("args[" ++ (toString i) ++ "]") t
    in
        List.indexedMap convertArg fn.params
            |> String.join ", "


wrapperName : Function -> String
wrapperName fn =
    "F" ++ toString (List.length fn.params)


wrapResult : Function -> String
wrapResult fn =
    -- TODO
    ""


convertType : Function -> String
convertType name t =
    -- TODO
    ""
