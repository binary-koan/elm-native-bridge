module Functions.Refine exposing (..)

import Ast.Statement exposing (..)
import Functions.Discover exposing (..)
import Types.Refine as T


type alias Function =
    { name : String
    , params : List T.Type
    , result : FunctionOutput
    }


type FunctionOutput
    = BasicOutput T.Type
    | ResultOutput T.Type T.Type
    | TaskOutput T.Type T.Type


type alias PartialTypeDef =
    Result String ( List T.Type, Maybe FunctionOutput )


refineFunctions : List DiscoveredFunction -> Result String (List Function)
refineFunctions fns =
    let
        functions =
            List.map refineFunction fns

        errors =
            List.filterMap error functions

        successes =
            List.filterMap Result.toMaybe functions
    in
        if List.isEmpty successes then
            Err ("No transformable function definitions found!\n" ++ String.join "\n" errors)
        else
            Ok successes


refineFunction : DiscoveredFunction -> Result String Function
refineFunction fn =
    let
        definition =
            refineTypeDefinition fn.functionType (Ok ( [], Nothing ))
    in
        case definition of
            Err e ->
                Err e

            Ok ( _, Nothing ) ->
                Err ("Did not find a return type for a function - this should never happen! " ++ toString fn)

            Ok ( params, Just output ) ->
                Ok { name = fn.name, params = params, result = output }


refineTypeDefinition : Type -> PartialTypeDef -> PartialTypeDef
refineTypeDefinition t result =
    case t of
        TypeApplication t next ->
            refineTypeDefinition next (addParamType t result)

        t1 ->
            addReturnType t1 result


addParamType : Type -> PartialTypeDef -> PartialTypeDef
addParamType def result =
    Result.map2 (\t ( params, output ) -> ( params ++ [ t ], output )) (parseType def) result


addReturnType : Type -> PartialTypeDef -> PartialTypeDef
addReturnType t result =
    Result.map2 (\t ( params, _ ) -> ( params, Just t )) (parseResult t) result


parseResult : Type -> Result String FunctionOutput
parseResult t =
    case t of
        TypeConstructor [ "Result" ] [ t1, t2 ] ->
            Result.map2 ResultOutput (parseType t1) (parseType t2)

        TypeConstructor [ "Task" ] [ t1, t2 ] ->
            Result.map2 TaskOutput (parseType t1) (parseType t2)

        t_ ->
            Result.map BasicOutput (parseType t_)


parseType : Type -> Result String T.Type
parseType t =
    case t of
        TypeConstructor [ "Bool" ] [] ->
            Ok T.BasicType

        TypeConstructor [ "Int" ] [] ->
            Ok T.BasicType

        TypeConstructor [ "Float" ] [] ->
            Ok T.BasicType

        TypeConstructor [ "String" ] [] ->
            Ok T.BasicType

        TypeConstructor [ "Maybe" ] [ t1 ] ->
            Result.map T.MaybeType (parseType t1)

        TypeConstructor [ "List" ] [ t1 ] ->
            Result.map T.ListType (parseType t1)

        TypeConstructor [ "Dict" ] [ TypeConstructor [ "String" ] [], t1 ] ->
            Result.map T.DictType (parseType t1)

        _ ->
            Err ("Unknown type definition: " ++ toString t)


error : Result String Function -> Maybe String
error result =
    case result of
        Err err ->
            Just err

        Ok _ ->
            Nothing
