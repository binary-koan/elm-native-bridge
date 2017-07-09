module Types.Generate exposing (..)

import Utils.Interpolate exposing (..)
import Functions.Generate exposing (..)
import Types.Refine exposing (..)


generateTypeConverters : Type -> String
generateTypeConverters t =
    case t of
        RecordType name fields ->
            generateRecordConverters name fields

        UnionType union ->
            generateUnionConverters union

        _ ->
            ""


generateRecordConverters : String -> List ( String, Type ) -> String
generateRecordConverters name fields =
    interpolate """
        typeConverters.jsToElm{0} = {1}
        typeConverters.elmToJs{0} = {2}
        """
        [ name, jsToElmRecordConverter fields, elmToJsRecordConverter fields ]


jsToElmRecordConverter : List ( String, Type ) -> String
jsToElmRecordConverter fields =
    let
        fieldConverter ( name, t ) =
            interpolate "elmValue.{0} = {1}" [ name, jsToElmValue ("original." ++ name) t ]

        body =
            List.map fieldConverter fields |> String.join ("\n")
    in
        interpolate """
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
            interpolate "jsValue.{0} = {1}" [ name, elmToJsValue ("original." ++ name) t ]

        body =
            List.map fieldConverter fields |> String.join ("\n")
    in
        interpolate """
            original => {
                var jsValue = Object.assign({}, original)
                {1}
                return jsValue
            }
            """ [ body ]


generateUnionConverters : UnionOptions -> String
generateUnionConverters union =
    interpolate """
        typeConverters.jsToElm{0} = value => {1}
        typeConverters.elmToJs{0} = value => {2}
        """
        [ name, jsToElmUnionConverter union.values union.defaultCtor, elmToJsUnionConverter union.values union.defaultJs ]


jsToElmUnionConverter : List ( String, String ) -> String -> String
jsToElmUnionConverter options default =
    let
        converter ( constructor, value ) =
            interpolate """
                if (value === {0}) {
                    return { ctor: "{1}" }
                }
                """
                [ value, constructor ]
    in
        interpolate """
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
            interpolate """
                if (value.ctor === "{0}") {
                    return {1}
                }
                """
                [ value, constructor ]
    in
        interpolate """
            value => {
                {1} else {
                    return {2}
                }
            }
            """
            [ String.join "\n" (List.map converter options), default ]