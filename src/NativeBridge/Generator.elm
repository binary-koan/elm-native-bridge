module NativeBridge.Generator exposing (..)

import Task exposing (..)
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
    , params : List Param
    , result : FieldType
    }


type alias GeneratedFiles =
    { elmFile : String
    , jsFile : String
    , features : Features
    }


type alias Features =
    { maybeTypes : Bool
    , listTypes : Bool
    , dictTypes : Bool
    }


generateFiles : String -> String -> List BridgeType -> List BridgeFunction -> Task String GeneratedFiles
generateFiles moduleName subject types functions =
    generateTypeDefinitions types
        |> andThen (generateFunctionDefinitions functions)
        |> andThen (addWrappers moduleName subject)



-- Basics


addWrappers : String -> String -> GeneratedFiles -> Task String GeneratedFiles
addWrappers moduleName subject files =
    succeed { files | elmFile = wrapElmModule moduleName files.elmFile, jsFile = wrapJsModule moduleName files.jsFile }


wrapElmModule : String -> String -> String
wrapElmModule moduleName content =
    "module " ++ moduleName ++ " exposing (..)\n\n" ++ content


wrapJsModule : String -> String -> String
wrapJsModule moduleName content =
    "var " ++ repoName ++ "$" ++ (toJsId moduleName) ++ " = function() {\n" ++ (indent content) ++ "\n}()\n"



-- Types


generateTypeDefinitions : List BridgeType -> Task String GeneratedFiles
generateTypeDefinitions types =
    succeed { elmFile = generateElmTypes types, jsFile = generateJsTypes types, features = detectFeatures types }


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



-- Functions


generateFunctionDefinitions : List BridgeFunction -> GeneratedFiles -> Task String GeneratedFiles
generateFunctionDefinitions functions files =
    succeed { files | elmFile = files.elmFile ++ "\n" ++ generateElmFunctions functions }


generateElmFunctions : List BridgeFunction -> String
generateElmFunctions functions =
    let
        formatParam param =
            case param of
                Dynamic t ->
                    Just (formatType t)

                _ ->
                    Nothing

        formatParams function =
            List.filterMap formatParam function.params
                |> String.join " -> "

        generateFunction function =
            function.elmName ++ " : " ++ (formatParams function) ++ " -> " ++ (formatType function.result)
    in
        List.map generateFunction functions |> String.join "\n\n"



-- Utilities


detectFeatures : List BridgeType -> Features
detectFeatures types =
    let
        fieldsFor t =
            case t of
                RecordType record ->
                    record.fields

                UnionType union ->
                    --TODO
                    []

        allFields =
            List.concatMap fieldsFor types

        featuresForField field features =
            case field.fieldType of
                ListField t ->
                    { features | listTypes = True }

                MaybeField t ->
                    { features | maybeTypes = True }

                DictField t ->
                    { features | dictTypes = True }

                _ ->
                    features
    in
        List.foldr featuresForField { listTypes = False, maybeTypes = False, dictTypes = False } allFields


formatType : FieldType -> String
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

        GeneratedField name ->
            name


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
