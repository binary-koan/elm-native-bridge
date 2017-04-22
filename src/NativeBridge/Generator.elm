module NativeBridge.Generator exposing (..)

import Task exposing (Task)
import NativeBridge.Fields exposing (..)
import NativeBridge.Params exposing (..)


type BridgeType
    = RecordType
        { name : String
        , fields : List Field
        }
    | UnionType
        { name : String
        , options : List ( String, BridgeType )
        }


type alias BridgeFunction =
    { name : String
    , elmName : String
    , definition : List String
    , prependParams : List FixedParam
    , appendParams : List FixedParam
    }


type alias GeneratedFiles =
    { elmFile : String
    , jsFile : String
    }


generateFiles : String -> String -> List BridgeType -> List BridgeFunction -> Task String GeneratedFiles
generateFiles moduleName subject types functions =
    Task.succeed
        { elmFile = wrapElmModule moduleName (generateElmTypes types)
        , jsFile = wrapJsModule moduleName (jsPreamble ++ "\n\n" ++ generateJsTypes types)
        }



-- Basics


wrapElmModule : String -> String -> String
wrapElmModule moduleName content =
    "module " ++ moduleName ++ " exposing (..)\n\n" ++ content


wrapJsModule : String -> String -> String
wrapJsModule moduleName content =
    "var " ++ repoName ++ "$" ++ (toJsId moduleName) ++ " = function() {\n" ++ (indent content) ++ "\n}()\n"



-- Types


generateElmTypes : List BridgeType -> String
generateElmTypes types =
    let
        generateRecordType record =
            "type alias " ++ record.name ++ " =\n" ++ (indent (generateElmFields record.fields))

        generateElmType t =
            case t of
                RecordType record ->
                    generateRecordType record

                UnionType union ->
                    --TODO
                    ""
    in
        (List.map generateElmType types |> String.join "\n") ++ "\n"


generateElmFields : List Field -> String
generateElmFields fields =
    let
        formatType fieldType =
            case fieldType of
                BoolField ->
                    "Bool"

                IntField ->
                    "Int"

                FloatField ->
                    "Float"

                StringField ->
                    "String"

                ListField t ->
                    "List (" ++ formatType t ++ ")"

                MaybeField t ->
                    "Maybe (" ++ formatType t ++ ")"

                DictField t ->
                    "Dict String (" ++ formatType t ++ ")"

        formatField field =
            field.name ++ " : " ++ formatType field.fieldType

        joinedFields =
            List.map formatField fields
                |> String.join "\n, "
    in
        "{ " ++ joinedFields ++ "\n}"


generateJsTypes : List BridgeType -> String
generateJsTypes types =
    let
        generateRecordType record =
            (jsTypeConverter "from" record.name record.fields) ++ "\n" ++ (jsTypeConverter "to" record.name record.fields)

        generateType t =
            case t of
                RecordType record ->
                    generateRecordType record

                UnionType union ->
                    -- TODO
                    ""
    in
        String.join "\n" (List.map generateType types)


jsTypeConverter : String -> String -> List Field -> String
jsTypeConverter direction name fields =
    let
        converterFor fieldType =
            case fieldType of
                ListField t ->
                    direction ++ "List(" ++ (converterFor t) ++ ")"

                MaybeField t ->
                    direction ++ "Maybe(" ++ (converterFor t) ++ ")"

                DictField t ->
                    direction ++ "Dict(" ++ (converterFor t) ++ ")"

                _ ->
                    ""

        fieldConverter field =
            if String.isEmpty (converterFor field.fieldType) then
                ""
            else
                "value." ++ field.name ++ " = " ++ converterFor field.fieldType ++ "(original." ++ field.name ++ ")"

        fieldConverters =
            List.map fieldConverter fields |> List.filter (not << String.isEmpty) |> String.join ("\n")

        body =
            if (String.isEmpty fieldConverters) then
                "return original"
            else
                "var value = Object.assign({}, original)\n" ++ fieldConverters ++ "\nreturn value"
    in
        "typeConverters." ++ (converterName direction name) ++ " = function(original) {\n" ++ (indent body) ++ "\n}"



-- Utilities


converterName : String -> String -> String
converterName direction name =
    direction ++ "Elm" ++ name


repoName : String
repoName =
    --TODO
    "_binary_koan$elm_native_bridge"


toJsId : String -> String
toJsId id =
    String.split "." id |> String.join "$"


indent : String -> String
indent lines =
    "    " ++ (String.split "\n" lines |> String.join "\n    ")


jsPreamble : String
jsPreamble =
    """
var typeConverters = {}
var functions = {}
    """ |> String.trim
