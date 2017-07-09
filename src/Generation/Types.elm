module Generation.Types exposing (findTypes)

import Ast.Statement exposing (..)
import Generation.Annotations exposing (..)


type alias GeneratedType =
    { annotations : List Annotation
    , declaration : Maybe Statement
    }


findTypes : List Statement -> List GeneratedType
findTypes statements =
    let
        findType stmt types =
            case stmt of
                Comment text ->
                    addAnnotations text types

                TypeDeclaration qualifier defs ->
                    addTypeDeclaration (TypeDeclaration qualifier defs) types

                TypeAliasDeclaration qualifier def ->
                    addTypeDeclaration (TypeAliasDeclaration qualifier def) types

                _ ->
                    removeAnnotation types
    in
        List.foldl findType [] statements


addAnnotations : String -> List GeneratedType -> List GeneratedType
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


addTypeDeclaration : Statement -> List GeneratedType -> List GeneratedType
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


firstType : List GeneratedType -> GeneratedType
firstType types =
    Maybe.withDefault emptyType (List.head types)


emptyType : GeneratedType
emptyType =
    { annotations = [], declaration = Nothing }


removeAnnotation : List GeneratedType -> List GeneratedType
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
