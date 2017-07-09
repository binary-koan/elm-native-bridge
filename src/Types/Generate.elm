module Types.Generate exposing (..)

import Utils.Format exposing (..)
import Functions.Generate exposing (..)
import Types.Refine exposing (..)


generateTypeConverters : List Type -> String
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


generateRecordConverters : String -> List ( String, Type ) -> String
generateRecordConverters name fields =
    format """
        typeConverters.jsToElm{0} = {1}
        typeConverters.elmToJs{0} = {2}
        """
        [ name, jsToElmRecordConverter fields, elmToJsRecordConverter fields ]


jsToElmRecordConverter : List ( String, Type ) -> String
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
                {1}
                return elmValue
            }
            """
            [ body ]


elmToJsRecordConverter : List ( String, Type ) -> String
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
                {1}
                return jsValue
            }
            """ [ body ]


generateUnionConverters : UnionOptions -> String
generateUnionConverters union =
    format """
        typeConverters.jsToElm{0} = value => {1}
        typeConverters.elmToJs{0} = value => {2}
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
                {1} else {
                    return {2}
                }
            }
            """
            [ String.join "\n" (List.map converter options), default ]


elmToJsUnionConverter : List ( String, String ) -> String -> String
elmToJsUnionConverter options default =
    let
        converter ( constructor, value ) =
            format """
                if (value.ctor === "{0}") {
                    return {1}
                }
                """
                [ value, constructor ]
    in
        format """
            value => {
                {1} else {
                    return {2}
                }
            }
            """
            [ String.join "\n" (List.map converter options), default ]
