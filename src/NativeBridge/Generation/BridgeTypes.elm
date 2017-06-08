module NativeBridge.Generation.BridgeTypes exposing (..)

import Result exposing (Result(..))
import Ast.Statement exposing (..)
import NativeBridge.Types exposing (..)


type BridgeType
    = BasicType
    | ListType BridgeType
    | MaybeType BridgeType
    | DictType BridgeType
    | GeneratedType String
    | RecordType (List ( String, BridgeType ))


parseTypeDefinition : Type -> BridgeGenerator -> Result String BridgeType
parseTypeDefinition typeDefinition generator =
    case typeDefinition of
        TypeConstructor [ t ] types ->
            parseTypeConstructor t types

        TypeRecord fields ->
            parseRecord fields

        _ ->
            Err ("Failed to parse type: " ++ toString typeDefinition)


parseTypeConstructor : String -> List Type -> Result String BridgeType
parseTypeConstructor name types =
    case ( name, types ) of
        ( "Bool", [] ) ->
            Ok BasicType

        ( "Int", [] ) ->
            Ok BasicType

        ( "Float", [] ) ->
            Ok BasicType

        ( "String", [] ) ->
            Ok BasicType

        _ ->
            Err ("Unknown type: " ++ name ++ " with params " ++ toString types)


parseRecord : List ( String, Type ) -> Result String BridgeType
parseRecord fields =
    Err "Not yet implemented: parseRecord"
