module Types.Refine exposing (BridgeType(..), UnionOptions, refineTypes, parseType)

import Ast.Statement exposing (..)
import Utils.Result exposing (..)
import Utils.Annotations exposing (..)
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
            refineUnion qualifier defs discovered.annotations

        Just (RecordDeclaration qualifier def) ->
            Result.map2 buildRecord (parseName qualifier) (parseRecord def)

        Nothing ->
            Err ("Incomplete type sneaked through (this should never happen): " ++ toString discovered)


refineUnion : Type -> List Type -> List Annotation -> Result String BridgeType
refineUnion qualifier defs annotations =
    let
        name =
            parseName qualifier

        options =
            joinUnionTypes annotations defs

        defaults =
            options |> Result.andThen (findUnionDefaults annotations)

        buildUnion name options ( defaultCtor, defaultJs ) =
            UnionType
                { name = name
                , values = options
                , defaultCtor = defaultCtor
                , defaultJs = defaultJs
                }
    in
        Result.map3 buildUnion name options defaults


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


joinUnionTypes : List Annotation -> List Type -> Result String (List ( String, String ))
joinUnionTypes annotations defs =
    let
        findName t =
            case t of
                TypeConstructor [ name ] [] ->
                    Ok name

                _ ->
                    Err "Union types must be enums without parameters"

        findNames =
            ensureAll findName defs

        findValues ns =
            findUnionValues ns annotations
    in
        findNames |> Result.andThen findValues


findUnionValues : List String -> List Annotation -> Result String (List ( String, String ))
findUnionValues names annotations =
    let
        escapeString value =
            "\"" ++ value ++ "\""

        matchParams params =
            case params of
                [ ( "", name ), ( "is", value ) ] ->
                    Ok <| Just ( name, value )

                [ ( "", name ), ( "is string", value ) ] ->
                    Ok <| Just ( name, escapeString value )

                _ ->
                    Err "Enum annotations must be of the form: `name` is `value` OR `name` is string `value`"

        unionValue annotation =
            if annotation.name == "enum" then
                matchParams annotation.params
            else
                Ok Nothing

        checkAllPresent values =
            if names == (List.map Tuple.first values) then
                Ok values
            else
                Err ("A JS representation of all union values must be specified. Compare union " ++ toString names ++ " with annotations " ++ toString values)
    in
        ensureAll unionValue annotations
            |> Result.map (List.filterMap identity)
            |> Result.andThen checkAllPresent


findUnionDefaults : List Annotation -> List ( String, String ) -> Result String ( String, String )
findUnionDefaults annotations options =
    let
        defaultAnnotation =
            List.head <| List.filter (\annotation -> annotation.name == "enum default") annotations

        nameParam annotation =
            case annotation.params of
                [ ( "", name ) ] ->
                    Just name

                _ ->
                    Nothing

        nameAndValue name =
            List.head <| List.filter (\( n, _ ) -> n == name) options
    in
        defaultAnnotation
            |> Maybe.andThen nameParam
            |> Maybe.andThen nameAndValue
            |> Result.fromMaybe "A default enum value must be specified, and must correspond to a type constructor"


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
