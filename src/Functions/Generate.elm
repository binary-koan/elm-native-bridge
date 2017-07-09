module Functions.Generate exposing (..)

import Utils.Interpolate exposing (..)
import Types.Refine exposing (..)
import Functions.Refine exposing (..)


generateFunction : Function -> String
generateFunction fn =
    interpolate """
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
            interpolate """
                var result = {0}(...args)
                return {1}
                """
                [ fn.name, jsToElmValue "result" t ]

        ResultOutput err ok ->
            interpolate """
                try {
                    var result = {0}(...args)
                    return {1}
                } catch(err) {
                    return {2}
                }
                """
                [ fn.name, jsToElmValue "result" ok, jsToElmValue "err" err ]

        TaskOutput err ok ->
            interpolate """
                return _elm_lang$core$Native_Scheduler.nativeBinding(callback => {
                    {0}(...args, function(err, result) {
                        if (!err) {
                            return {1}
                        } else {
                            return {2}
                        }
                    })
                })
                """
                [ fn.name, jsToElmValue "result" ok, jsToElmValue "err" err ]


elmToJsValue : String -> Type -> String
elmToJsValue varName t =
    case t of
        ListType t ->
            interpolate "_elm_lang$core$Native_List.toArray({0}){1}" [ varName, listValuesToJs t ]

        MaybeType t ->
            interpolate "{0}.ctor === 'Just' ? {1} : null" [ varName, elmToJsValue (varName ++ "._0") t ]

        DictType t ->
            interpolate
                """
                _elm_lang$core$Dict$foldr(F3((key, value, obj) => {
                    obj[key] = {0}
                    return obj
                }), {}, {1})
                """
                [ elmToJsValue "value" t, varName ]

        RecordType name _ ->
            interpolate "elmToJs{0}({1})" [ name, varName ]

        UnionType name _ ->
            interpolate "elmToJs{0}({1})" [ name, varName ]

        _ ->
            ""


jsToElmValue : String -> Type -> String
jsToElmValue varName t =
    case t of
        ListType t ->
            interpolate "_elm_lang$core$Native_List.fromArray({0})" [ arrayToElmValues varName t ]

        MaybeType t ->
            interpolate "{0} == null ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just({1})"
                [ varName, jsToElmValue varName t ]

        DictType t ->
            interpolate
                """
                (() => {
                    var dict = _elm_lang$core$Dict$empty
                    Object.keys({0}).forEach(key => dict = _elm_lang$core$Dict$insert(key)({1})(dict))
                    return dict
                })()
                """
                [ varName, jsToElmValue "dict[key]" t ]

        RecordType name _ ->
            interpolate "jsToElm{0}({1})" [ name, varName ]

        UnionType name _ ->
            interpolate "jsToElm{0}({1})" [ name, varName ]

        BasicType ->
            varName


arrayToElmValues : String -> Type -> String
arrayToElmValues varName contentType =
    varName ++ ".map(value => " ++ (jsToElmValue "value" contentType) ++ ")"


listValuesToJs : Type -> String
listValuesToJs contentType =
    ".map(value => " ++ (elmToJsValue "value" contentType) ++ ")"
