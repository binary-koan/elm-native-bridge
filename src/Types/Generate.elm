module Types.Generate exposing (generateTypeConverters, jsToElmValue, elmToJsValue)

import Utils.Format exposing (..)
import Types.Refine exposing (..)


generateTypeConverters : List BridgeType -> String
generateTypeConverters types =
    let
        converters t =
            case t of
                RecordType name fields ->
                    generateRecordConverters name fields

                UnionType union ->
                    generateUnionConverters union

                _ ->
                    ""
    in
        String.join "\n" (List.map converters types)


jsToElmValue : String -> BridgeType -> String
jsToElmValue varName t =
    case t of
        ListType t ->
            format "_elm_lang$core$Native_List.fromArray({0})" [ arrayToElmValues varName t ]

        MaybeType t ->
            format "{0} == null ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just({1})"
                [ varName, jsToElmValue varName t ]

        DictType t ->
            format
                """
                (() => {
                    var dict = _elm_lang$core$Dict$empty
                    Object.keys({0}).forEach(key => dict = _elm_lang$core$Dict$insert(key)({1})(dict))
                    return dict
                })()
                """
                [ varName, jsToElmValue "dict[key]" t ]

        RecordType name _ ->
            format "jsToElm{0}({1})" [ name, varName ]

        UnionType union ->
            format "jsToElm{0}({1})" [ union.name, varName ]

        BasicType ->
            varName

        UnitType ->
            "{ ctor: '_Tuple0' }"


arrayToElmValues : String -> BridgeType -> String
arrayToElmValues varName contentType =
    varName ++ ".map(value => " ++ (jsToElmValue "value" contentType) ++ ")"


elmToJsValue : String -> BridgeType -> String
elmToJsValue varName t =
    case t of
        ListType t ->
            format "_elm_lang$core$Native_List.toArray({0}){1}" [ varName, listValuesToJs t ]

        MaybeType t ->
            format "{0}.ctor === 'Just' ? {1} : null" [ varName, elmToJsValue (varName ++ "._0") t ]

        DictType t ->
            format
                """
                _elm_lang$core$Dict$foldr(F3((key, value, obj) => {
                    obj[key] = {0}
                    return obj
                }), {}, {1})
                """
                [ elmToJsValue "value" t, varName ]

        RecordType name _ ->
            format "elmToJs{0}({1})" [ name, varName ]

        UnionType union ->
            format "elmToJs{0}({1})" [ union.name, varName ]

        BasicType ->
            varName

        UnitType ->
            "null"


listValuesToJs : BridgeType -> String
listValuesToJs contentType =
    ".map(value => " ++ (elmToJsValue "value" contentType) ++ ")"


generateRecordConverters : String -> List ( String, BridgeType ) -> String
generateRecordConverters name fields =
    format """
        typeConverters.jsToElm{0} = {1}
        typeConverters.elmToJs{0} = {2}
        """
        [ name, jsToElmRecordConverter fields, elmToJsRecordConverter fields ]


jsToElmRecordConverter : List ( String, BridgeType ) -> String
jsToElmRecordConverter fields =
    let
        fieldConverter ( name, t ) =
            format "elmValue.{0} = {1}" [ name, jsToElmValue ("original." ++ name) t ]

        body =
            List.map fieldConverter fields |> String.join ("\n")
    in
        format """
            original => {
                var elmValue = Object.assign({}, original)
                {0}
                return elmValue
            }
            """
            [ body ]


elmToJsRecordConverter : List ( String, BridgeType ) -> String
elmToJsRecordConverter fields =
    let
        fieldConverter ( name, t ) =
            format "jsValue.{0} = {1}" [ name, elmToJsValue ("original." ++ name) t ]

        body =
            List.map fieldConverter fields |> String.join ("\n")
    in
        format """
            original => {
                var jsValue = Object.assign({}, original)
                {0}
                return jsValue
            }
            """ [ body ]


generateUnionConverters : UnionOptions -> String
generateUnionConverters union =
    format """
        typeConverters.jsToElm{0} = {1}
        typeConverters.elmToJs{0} = {2}
        """
        [ union.name
        , jsToElmUnionConverter union.values union.defaultCtor
        , elmToJsUnionConverter union.values union.defaultJs
        ]


jsToElmUnionConverter : List ( String, String ) -> String -> String
jsToElmUnionConverter options default =
    let
        converter ( constructor, value ) =
            format """
                if (value === {0}) {
                    return { ctor: "{1}" }
                }
                """
                [ value, constructor ]
    in
        format """
            value => {
                {0} else {
                    return { ctor: "{1}" }
                }
            }
            """
            [ String.join " else " (List.map converter options), default ]


elmToJsUnionConverter : List ( String, String ) -> String -> String
elmToJsUnionConverter options default =
    let
        converter ( constructor, value ) =
            format """
                if (value.ctor === "{0}") {
                    return {1}
                }
                """
                [ constructor, value ]
    in
        format """
            value => {
                {0} else {
                    return {1}
                }
            }
            """
            [ String.join " else " (List.map converter options), default ]
