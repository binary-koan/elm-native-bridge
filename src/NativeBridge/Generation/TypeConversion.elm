module NativeBridge.Generation.TypeConversion exposing (..)

import String.Interpolate exposing (..)
import Ast.Statement exposing (..)
import NativeBridge.Types exposing (..)
import NativeBridge.Generation.BridgeTypes exposing (..)


jsToElmConverter : String -> BridgeType -> String
jsToElmConverter varName bridgeType =
    case bridgeType of
        ListType t ->
            interpolate "_elm_lang$core$Native_List.fromArray({0})" [ arrayToElmValues varName t ]

        MaybeType t ->
            interpolate "{0} == null ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just({1})"
                [ varName, jsToElmConverter varName t ]

        DictType t ->
            interpolate
                """
                (() => {
                    var dict = _elm_lang$core$Dict$empty
                    Object.keys({0}).forEach(key => dict = _elm_lang$core$Dict$insert(key)({1})(dict))
                    return dict
                })()
                """
                [ varName, jsToElmConverter "dict[key]" t ]

        GeneratedType name ->
            interpolate "jsToElm{0}({1})" [ name, varName ]

        _ ->
            varName


elmToJsConverter : String -> BridgeType -> String
elmToJsConverter varName bridgeType =
    case bridgeType of
        ListType t ->
            interpolate "_elm_lang$core$Native_List.toArray({0}){1}" [ varName, listValuesToJs t ]

        MaybeType t ->
            interpolate "{0}.ctor === 'Just' ? {1} : null" [ varName, elmToJsConverter (varName ++ "._0") t ]

        DictType t ->
            interpolate
                """
                _elm_lang$core$Dict$foldr(F3((key, value, obj) => {
                    obj[key] = {0}
                    return obj
                }), {}, {1})
                """
                [ elmToJsConverter "value" t, varName ]

        GeneratedType name ->
            interpolate "elmToJs{0}({1})" [ name, varName ]

        _ ->
            ""


arrayToElmValues : String -> BridgeType -> String
arrayToElmValues varName contentType =
    case jsToElmConverter "value" contentType of
        "value" ->
            varName

        converter ->
            varName ++ ".map(value => " ++ converter ++ ")"


listValuesToJs : BridgeType -> String
listValuesToJs contentType =
    case elmToJsConverter "value" contentType of
        "value" ->
            ""

        converter ->
            ".map(value => " ++ converter ++ ")"
