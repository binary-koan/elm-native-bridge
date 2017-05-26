module NativeBridge.ConverterGenerator exposing (..)

import NativeBridge.Fields exposing (..)


jsToElmConverter : FieldType -> String
jsToElmConverter fieldType =
    case fieldType of
        ListField t ->
            "_elm_lang$core$Native_List.fromArray(" ++ arrayToElmValues t ++ ")"

        MaybeField t ->
            "value == null ? _elm_lang$core$Maybe$Nothing : _elm_lang$core$Maybe$Just(" ++ valueToElm t ++ ")"

        DictField t ->
            "(() => {\n  var dict = _elm_lang$core$Dict$empty\n"
                ++ "  Object.keys(value).forEach(key => dict = _elm_lang$core$Dict$insert(key)("
                ++ valueToElm t
                ++ ")(dict))\n"
                ++ "  return dict\n})()"

        _ ->
            ""


elmToJsConverter : FieldType -> String
elmToJsConverter fieldType =
    case fieldType of
        ListField t ->
            "_elm_lang$core$Native_List.toArray(value)" ++ listValuesToJs t

        MaybeField t ->
            "value.ctor === 'Just' ? " ++ justValueToJs t ++ " : null"

        DictField t ->
            "_elm_lang$core$Dict$foldr(F3((key, value, obj) => {\n"
                ++ "  obj[key] = "
                ++ valueToJs t
                ++ "\n  return obj\n"
                ++ "}, {}, value)"

        _ ->
            ""


arrayToElmValues : FieldType -> String
arrayToElmValues contentType =
    if String.isEmpty (jsToElmConverter contentType) then
        "value"
    else
        "value.map(value => " ++ jsToElmConverter contentType ++ ")"


listValuesToJs : FieldType -> String
listValuesToJs contentType =
    if String.isEmpty (elmToJsConverter contentType) then
        ""
    else
        ".map(value => " ++ elmToJsConverter contentType ++ ")"


valueToElm : FieldType -> String
valueToElm contentType =
    if String.isEmpty (jsToElmConverter contentType) then
        "value"
    else
        jsToElmConverter contentType


valueToJs : FieldType -> String
valueToJs contentType =
    if String.isEmpty (elmToJsConverter contentType) then
        "value"
    else
        elmToJsConverter contentType


justValueToJs : FieldType -> String
justValueToJs contentType =
    if String.isEmpty (elmToJsConverter contentType) then
        "value._0"
    else
        "(value => " ++ elmToJsConverter contentType ++ ")(value._0)"
