module Functions.Generate exposing (..)

import Utils.Format exposing (..)
import Functions.Refine exposing (..)
import Types.Generate exposing (..)


generateFunctions : List Function -> String
generateFunctions fns =
    String.join "\n" (List.map generateFunction fns)


generateFunction : Function -> String
generateFunction fn =
    format """
        functions.{0} = {1}((...args) => {
            args = [{2}]
            {3}
        })
        """
        [ fn.name, wrapperName fn, generateArgs fn, wrapOutput fn ]


generateArgs : Function -> String
generateArgs fn =
    let
        convertArg i t =
            elmToJsValue ("args[" ++ (toString i) ++ "]") t
    in
        List.indexedMap convertArg fn.params
            |> String.join ", "


wrapperName : Function -> String
wrapperName fn =
    "F" ++ toString (List.length fn.params)


wrapOutput : Function -> String
wrapOutput fn =
    case fn.result of
        BasicOutput t ->
            format """
                var result = {0}(...args)
                return {1}
                """
                [ qualifiedName fn, jsToElmValue "result" t ]

        ResultOutput err ok ->
            format """
                try {
                    var result = {0}(...args)
                    return {1}
                } catch(err) {
                    return {2}
                }
                """
                [ qualifiedName fn, jsToElmValue "result" ok, jsToElmValue "err" err ]

        TaskOutput err ok ->
            format """
                return _elm_lang$core$Native_Scheduler.nativeBinding(callback => {
                    {0}(...args, function(err, result) {
                        if (!err) {
                            callback({1})
                        } else {
                            callback({2})
                        }
                    })
                })
                """
                [ qualifiedName fn, jsToElmValue "result" ok, jsToElmValue "err" err ]


qualifiedName : Function -> String
qualifiedName fn =
    "subject." ++ fn.name
