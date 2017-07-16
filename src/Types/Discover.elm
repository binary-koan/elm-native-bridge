module Types.Discover exposing (findTypes, DiscoveredType, DiscoveredDeclaration(..))

import Ast.Statement exposing (..)
import Utils.Annotations exposing (..)


type alias DiscoveredType =
    { annotations : List Annotation
    , declaration : Maybe DiscoveredDeclaration
    }


type DiscoveredDeclaration
    = UnionDeclaration Type (List Type)
    | RecordDeclaration Type Type


findTypes : List Statement -> List DiscoveredType
findTypes statements =
    let
        findType stmt types =
            case stmt of
                Comment text ->
                    addAnnotations text types

                TypeDeclaration qualifier defs ->
                    addTypeDeclaration (UnionDeclaration qualifier defs) types

                TypeAliasDeclaration qualifier def ->
                    addTypeDeclaration (RecordDeclaration qualifier def) types

                _ ->
                    removeAnnotation types
    in
        List.foldl findType [] statements


addAnnotations : String -> List DiscoveredType -> List DiscoveredType
addAnnotations text types =
    let
        addAnnotationsTo t =
            { t | annotations = t.annotations ++ findAnnotations text }
    in
        case (firstType types).declaration of
            Nothing ->
                (addAnnotationsTo (firstType types)) :: Maybe.withDefault [] (List.tail types)

            Just declaration ->
                (addAnnotationsTo emptyType) :: types


addTypeDeclaration : DiscoveredDeclaration -> List DiscoveredType -> List DiscoveredType
addTypeDeclaration declaration types =
    let
        first =
            firstType types
    in
        case ( types, first.declaration ) of
            ( [], _ ) ->
                [ { first | declaration = Just declaration } ]

            ( _, Nothing ) ->
                { first | declaration = Just declaration } :: Maybe.withDefault [] (List.tail types)

            _ ->
                { emptyType | declaration = Just declaration } :: types


firstType : List DiscoveredType -> DiscoveredType
firstType types =
    Maybe.withDefault emptyType (List.head types)


emptyType : DiscoveredType
emptyType =
    { annotations = [], declaration = Nothing }


removeAnnotation : List DiscoveredType -> List DiscoveredType
removeAnnotation types =
    let
        firstType =
            Maybe.withDefault emptyType (List.head types)
    in
        case firstType.annotations of
            [] ->
                types

            _ ->
                Maybe.withDefault [] (List.tail types)
