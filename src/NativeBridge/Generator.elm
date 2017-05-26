module NativeBridge.Generator exposing (..)

import Task exposing (..)
import NativeBridge.Fields exposing (..)
import NativeBridge.Params exposing (..)
import NativeBridge.ConverterGenerator exposing (..)


type BridgeEntity
    = Record RecordOptions
    | Union UnionOptions
    | Function FunctionOptions


type alias RecordOptions =
    { name : String
    , fields : List Field
    }


type alias UnionOptions =
    { name : String
    , options : List ( String, BridgeEntity )
    }


type alias FunctionOptions =
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


generateFiles : String -> String -> List BridgeEntity -> Task String GeneratedFiles
generateFiles moduleName subject types =
    generateEntities types
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


generateEntities : List BridgeEntity -> Task String GeneratedFiles
generateEntities entities =
    succeed
        { elmFile = String.join "\n\n" (List.map generateElmEntity entities)
        , jsFile = String.join "\n\n" (List.map generateJsEntity entities)
        , features = detectFeatures entities
        }


generateElmEntity : BridgeEntity -> String
generateElmEntity entity =
    case entity of
        Record record ->
            generateElmRecord record

        Union union ->
            generateElmUnion union

        Function function ->
            generateElmFunction function


generateJsEntity : BridgeEntity -> String
generateJsEntity entity =
    case entity of
        Record record ->
            generateJsRecord record

        Union union ->
            generateJsUnion union

        Function function ->
            generateJsFunction function



-- Record types


generateElmRecord : RecordOptions -> String
generateElmRecord record =
    let
        formatField field =
            field.name ++ " : " ++ formatType field.fieldType

        formattedFields fields =
            "{ " ++ String.join "\n, " (List.map formatField fields) ++ "\n}"
    in
        "type alias " ++ record.name ++ " =\n" ++ (indent (formattedFields record.fields))


generateJsRecord : RecordOptions -> String
generateJsRecord record =
    (elmToJsRecordConverter record.name record.fields) ++ "\n" ++ (jsToElmRecordConverter record.name record.fields)


elmToJsRecordConverter : String -> List Field -> String
elmToJsRecordConverter name fields =
    let
        fieldConverter field =
            if String.isEmpty (elmToJsConverter field.fieldType) then
                ""
            else
                "value = original."
                    ++ field.name
                    ++ "; "
                    ++ "jsValue."
                    ++ field.name
                    ++ " = "
                    ++ elmToJsConverter field.fieldType

        fieldConverters =
            List.map fieldConverter fields |> List.filter (not << String.isEmpty) |> String.join ("\n")

        body =
            if (String.isEmpty fieldConverters) then
                "return original"
            else
                "var jsValue = Object.assign({}, original)\n" ++ fieldConverters ++ "\nreturn jsValue"
    in
        "typeConverters.elmToJs" ++ name ++ " = function(original) {\n" ++ (indent body) ++ "\n}"


jsToElmRecordConverter : String -> List Field -> String
jsToElmRecordConverter name fields =
    let
        fieldConverter field =
            if String.isEmpty (jsToElmConverter field.fieldType) then
                ""
            else
                "value = original."
                    ++ field.name
                    ++ "; "
                    ++ "elmValue."
                    ++ field.name
                    ++ " = "
                    ++ jsToElmConverter field.fieldType

        fieldConverters =
            List.map fieldConverter fields |> List.filter (not << String.isEmpty) |> String.join ("\n")

        body =
            if (String.isEmpty fieldConverters) then
                "return original"
            else
                "var elmValue = Object.assign({}, original)\n" ++ fieldConverters ++ "\nreturn elmValue"
    in
        "typeConverters.jsToElm" ++ name ++ " = function(original) {\n" ++ (indent body) ++ "\n}"



-- Union types


generateElmUnion : UnionOptions -> String
generateElmUnion union =
    --TODO
    ""


generateJsUnion : UnionOptions -> String
generateJsUnion union =
    -- TODO
    ""



-- Functions


generateElmFunction : FunctionOptions -> String
generateElmFunction function =
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
    in
        function.elmName ++ " : " ++ (formatParams function) ++ " -> " ++ (formatType function.result)


generateJsFunction : FunctionOptions -> String
generateJsFunction function =
    ""


detectFeatures : List BridgeEntity -> Features
detectFeatures types =
    let
        fieldsFor t =
            case t of
                Record record ->
                    record.fields

                Union union ->
                    --TODO
                    []

                Function function ->
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
