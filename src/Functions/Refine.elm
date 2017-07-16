module Functions.Refine exposing (..)

import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import Utils.Result exposing (..)
import Functions.Discover exposing (..)
import Types.Refine exposing (..)


type alias Function =
    { name : String
    , params : List BridgeType
    , result : FunctionOutput
    }


type FunctionOutput
    = BasicOutput BridgeType
    | ResultOutput BridgeType BridgeType
    | TaskOutput BridgeType BridgeType


type alias PartialTypeDef =
    Result String ( List BridgeType, Maybe FunctionOutput )


refineFunctions : String -> List DiscoveredFunction -> Result String (List Function)
refineFunctions moduleName fns =
    let
        functions =
            List.map (refineFunction moduleName) fns

        errors =
            List.filterMap errToMaybe functions

        successes =
            List.filterMap Result.toMaybe functions
    in
        if List.isEmpty successes then
            Err ("No transformable function definitions found!\n" ++ String.join "\n" errors)
        else
            Ok successes


refineFunction : String -> DiscoveredFunction -> Result String Function
refineFunction moduleName fn =
    let
        nativeName =
            validNativeFunctionName moduleName fn.params fn.body

        definition =
            refineTypeDefinition fn.functionType (Ok ( [], Nothing ))

        buildFunction params output =
            Result.map (\name -> { name = name, params = params, result = output }) nativeName
    in
        case definition of
            Err e ->
                Err e

            Ok ( _, Nothing ) ->
                Err ("Did not find a return type for a function - this should never happen! " ++ toString fn)

            Ok ( params, Just output ) ->
                buildFunction params output


validNativeFunctionName : String -> List Expression -> Expression -> Result String String
validNativeFunctionName moduleName params body =
    let
        paramName param =
            case param of
                Variable [ name ] ->
                    Just name

                _ ->
                    Nothing

        foundParams =
            List.filterMap paramName params

        paramNames =
            if List.length foundParams == List.length params then
                Ok foundParams
            else
                Err "Parameters may only be variable names"

        nativeFunctionName body =
            case body of
                Application callee _ ->
                    nativeFunctionName callee

                Access (Variable [ "Native", modName ]) [ fName ] ->
                    if modName == moduleName then
                        Ok fName
                    else
                        Err "You cannot call native functions from another module"

                _ ->
                    Err "The function body must be a call to a native function"

        nativeFunctionParams body =
            case body of
                Application callee (Variable [ name ]) ->
                    Result.map (\names -> names ++ [ name ]) (nativeFunctionParams callee)

                Access _ _ ->
                    Ok []

                _ ->
                    Err "The function body must be a call to a native function"

        matchParams paramNames fnParams =
            if paramNames == fnParams then
                Ok ()
            else
                Err "Function parameters must be passed immediately to the native function"
    in
        Result.map2 matchParams paramNames (nativeFunctionParams body)
            |> Result.map2 (\name _ -> name) (nativeFunctionName body)


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
