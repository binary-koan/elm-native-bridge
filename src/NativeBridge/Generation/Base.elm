module NativeBridge.Generation.Base exposing (..)

import Result exposing (Result(..))
import String.Interpolate exposing (..)
import NativeBridge.Types exposing (..)


type alias GeneratedFiles =
    { elmFile : String
    , jsFile : String
    }


generateFiles : BridgeGenerator -> Result String GeneratedFiles
generateFiles generator =
    case ( generateElmFile generator, generateJsFile generator ) of
        ( Ok elmFile, Ok jsFile ) ->
            Ok { elmFile = elmFile, jsFile = jsFile }

        ( Err elmErr, _ ) ->
            Err ("Error generating Elm file: " ++ elmErr)

        ( _, Err jsErr ) ->
            Err ("Error generating JS file: " ++ jsErr)



-- Wrappers


generateElmFile : BridgeGenerator -> Result String String
generateElmFile generator =
    let
        wrapDefinitions typeDefs functionDefs =
            interpolate
                """
                module {0} exposing (..)
                import Native.{0}

                {1}
                {2}
                """
                [ generator.moduleName, typeDefs, functionDefs ]
    in
        Result.map2 wrapDefinitions (generateElmTypes generator) (generateElmFunctions generator)


generateJsFile : BridgeGenerator -> Result String String
generateJsFile generator =
    let
        wrapDefinitions typeDefs functionDefs =
            interpolate
                """
                var {0}${1} = (() => {
                    var typeConverters = {}, functions = {}
                    var context = {2}

                    {3}
                    {4}
                    return functions
                })()
                """
                [ generator.repoName, toJsId generator.moduleName, generator.subject, typeDefs, functionDefs ]
    in
        Result.map2 wrapDefinitions (generateJsTypes generator) (generateJsFunctions generator)



-- Types


generateElmTypes : BridgeGenerator -> Result String String
generateElmTypes generator =
    Ok <| String.join "\n" (List.map generateElmType generator.types)


generateElmType : TypeDefinition -> String
generateElmType typeDef =
    case typeDef of
        Union u ->
            interpolate "type {0}\n    = {1}\n"
                [ u.name
                , String.join "\n    | " (List.map Tuple.first u.options)
                ]

        Record r ->
            interpolate "type alias {0} =\n    { {1}\n    }\n"
                [ r.name, String.join "\n    ," r.fields ]


generateJsTypes : BridgeGenerator -> Result String String
generateJsTypes generator =
    Ok "// TODO types"



-- Functions


generateElmFunctions : BridgeGenerator -> Result String String
generateElmFunctions generator =
    Ok "-- TODO functions"


generateJsFunctions : BridgeGenerator -> Result String String
generateJsFunctions generator =
    Ok "// TODO functions"



-- Record types
-- generateJsRecord : RecordOptions -> String
-- generateJsRecord record =
--     (elmToJsRecordConverter record.name record.fields) ++ "\n" ++ (jsToElmRecordConverter record.name record.fields)
-- elmToJsRecordConverter : String -> List Field -> String
-- elmToJsRecordConverter name fields =
--     let
--         fieldConverter field =
--             if String.isEmpty (elmToJsConverter field.fieldType) then
--                 ""
--             else
--                 interpolate "value = original.{0}; jsValue.{0} = {1}" [ field.name, elmToJsConverter field.fieldType ]
--         fieldConverters =
--             List.map fieldConverter fields |> List.filter (not << String.isEmpty) |> String.join ("\n")
--         body =
--             if (String.isEmpty fieldConverters) then
--                 "return original"
--             else
--                 "var jsValue = Object.assign({}, original)\n" ++ fieldConverters ++ "\nreturn jsValue"
--     in
--         interpolate "typeConverters.elmToJs{0} = original => {\n{1}\n}\n" [ name, indent body ]
-- jsToElmRecordConverter : String -> List Field -> String
-- jsToElmRecordConverter name fields =
--     let
--         fieldConverter field =
--             if String.isEmpty (jsToElmConverter field.fieldType) then
--                 ""
--             else
--                 interpolate "value = original.{0}; elmValue.{0} = {1}" [ field.name, jsToElmConverter field.fieldType ]
--         fieldConverters =
--             List.map fieldConverter fields |> List.filter (not << String.isEmpty) |> String.join ("\n")
--         body =
--             if (String.isEmpty fieldConverters) then
--                 "return original"
--             else
--                 "var elmValue = Object.assign({}, original)\n" ++ fieldConverters ++ "\nreturn elmValue"
--     in
--         interpolate "typeConverters.jsToElm{0} = original => {\n{1}\n}\n" [ name, indent body ]
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
-- generateElmFunction : String -> FunctionOptions -> String
-- generateElmFunction moduleName function =
--     let
--         formatParam param =
--             case param of
--                 Dynamic t ->
--                     Just (formatType t)
--                 _ ->
--                     Nothing
--         formatParams =
--             List.filterMap formatParam function.params
--                 |> String.join " -> "
--         paramNames =
--             List.range 1 (List.length function.params)
--                 |> List.map (\i -> "p" ++ toString i)
--                 |> String.join " "
--     in
--         interpolate "{0} : {1} -> {2}\n{0} {3} = Native.{4}.{5} {3}"
--             [ function.elmName
--             , formatParams
--             , formatType function.result
--             , paramNames
--             , moduleName
--             , function.name
--             ]
-- generateJsFunction : FunctionOptions -> String
-- generateJsFunction function =
--     let
--         param p =
--             case p of
--                 Dynamic fieldType ->
--                     elmParam fieldType
--                 FixedBool b ->
--                     if b == True then
--                         "true"
--                     else
--                         "false"
--                 FixedInt i ->
--                     toString i
--                 FixedFloat f ->
--                     toString f
--                 FixedString s ->
--                     "\"" ++ (s |> String.split "\"" |> String.join "\\\"") ++ "\""
--                 FixedList l ->
--                     "[" ++ String.join ", " (List.map param l) ++ "]"
--                 FixedObject o ->
--                     "{" ++ String.join ", " (List.map (\( k, v ) -> k ++ ": " ++ param v) o) ++ "}"
--                 FixedNull ->
--                     "null"
--                 FixedUndefined ->
--                     "undefined"
--         elmParam fieldType =
--             if String.isEmpty (elmToJsConverter fieldType) then
--                 "args.shift()"
--             else
--                 "(value => " ++ elmToJsConverter fieldType ++ ")(args.shift())"
--         params =
--             String.join ",\n" (List.map param function.params)
--     in
--         interpolate """
-- functions.{0} = (...args) => {
--   var value = context.{0}(
-- {1}
--   )
--   return {2}
-- }
-- """ [ function.name, indent (indent params), valueToElm function.result ]
-- detectFeatures : List BridgeEntity -> Features
-- detectFeatures types =
--     let
--         fieldsFor t =
--             case t of
--                 Record record ->
--                     record.fields
--                 Union union ->
--                     --TODO
--                     []
--                 Function function ->
--                     []
--         allFields =
--             List.concatMap fieldsFor types
--         featuresForField field features =
--             case field.fieldType of
--                 ListField t ->
--                     { features | listTypes = True }
--                 MaybeField t ->
--                     { features | maybeTypes = True }
--                 DictField t ->
--                     { features | dictTypes = True }
--                 _ ->
--                     features
--     in
--         List.foldr featuresForField { listTypes = False, maybeTypes = False, dictTypes = False } allFields
-- formatType : FieldType -> String
-- formatType fieldType =
--     case fieldType of
--         BoolField ->
--             "Bool"
--         IntField ->
--             "Int"
--         FloatField ->
--             "Float"
--         StringField ->
--             "String"
--         ListField t ->
--             "List (" ++ formatType t ++ ")"
--         MaybeField t ->
--             "Maybe (" ++ formatType t ++ ")"
--         DictField t ->
--             "Dict String (" ++ formatType t ++ ")"
--         GeneratedField name ->
--             name
-- converterName : String -> String -> String
-- converterName direction name =
--     direction ++ "Elm" ++ name


repoName : String
repoName =
    --TODO
    "_binary_koan$elm_native_bridge"


toJsId : String -> String
toJsId id =
    String.split "." id |> String.join "$"


indent : String -> String
indent lines =
    "  " ++ (String.split "\n" lines |> String.join "\n  ")
