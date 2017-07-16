module Types.Refine exposing (BridgeType(..), UnionOptions, refineTypes, parseType)

import Ast.Statement exposing (..)
import Utils.Result exposing (..)
import Types.Discover exposing (..)


type BridgeType
    = BasicType
    | MaybeType BridgeType
    | ListType BridgeType
    | DictType BridgeType
    | UnionType UnionOptions
    | RecordType String (List ( String, BridgeType ))


type alias UnionOptions =
    { name : String
    , values : List ( String, String )
    , defaultCtor : String
    , defaultJs : String
    }


refineTypes : List DiscoveredType -> Result String (List BridgeType)
refineTypes discovered =
    Ok <| List.filterMap (refineType >> Result.toMaybe) discovered


parseType : Type -> Result String BridgeType
parseType t =
    case t of
        TypeConstructor [ "Bool" ] [] ->
            Ok BasicType

        TypeConstructor [ "Int" ] [] ->
            Ok BasicType

        TypeConstructor [ "Float" ] [] ->
            Ok BasicType

        TypeConstructor [ "String" ] [] ->
            Ok BasicType

        TypeConstructor [ "Maybe" ] [ t1 ] ->
            Result.map MaybeType (parseType t1)

        TypeConstructor [ "List" ] [ t1 ] ->
            Result.map ListType (parseType t1)

        TypeConstructor [ "Dict" ] [ TypeConstructor [ "String" ] [], t1 ] ->
            Result.map DictType (parseType t1)

        _ ->
            Err ("Unknown type definition: " ++ toString t)


refineType : DiscoveredType -> Result String BridgeType
refineType discovered =
    case discovered.declaration of
        Just (UnionDeclaration qualifier defs) ->
            Result.map2 buildUnion (parseName qualifier) (parseUnionTypes defs)

        Just (RecordDeclaration qualifier def) ->
            Result.map2 buildRecord (parseName qualifier) (parseRecord def)

        Nothing ->
            Err ("Incomplete type sneaked through (this should never happen): " ++ toString discovered)


buildUnion : String -> List String -> BridgeType
buildUnion name options =
    UnionType
        { name = name
        , values = List.map (\v -> ( v, "0 /*TODO parse value */" )) options
        , defaultCtor = Maybe.withDefault "0 /* TODO default? */" (List.head options)
        , defaultJs = "0 /* TODO */"
        }


buildRecord : String -> List ( String, BridgeType ) -> BridgeType
buildRecord name fields =
    RecordType name fields


parseName : Type -> Result String String
parseName t =
    case t of
        TypeConstructor [ name ] [] ->
            Ok name

        _ ->
            Err "Type name must be a single id"


parseUnionTypes : List Type -> Result String (List String)
parseUnionTypes types =
    let
        parseType t =
            case t of
                TypeConstructor [ name ] [] ->
                    Ok name

                _ ->
                    Err "Union types must be enums without parameters"
    in
        ensureAll parseType types


parseRecord : Type -> Result String (List ( String, BridgeType ))
parseRecord t =
    let
        parseField ( name, t ) =
            Result.map (\bridgeType -> ( name, bridgeType )) (parseType t)
    in
        case t of
            TypeRecord fields ->
                ensureAll parseField fields

            _ ->
                Err "Type alias declarations must be for record types"
