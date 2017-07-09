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
                    addTypeDeclaration qualifier defs types

                TypeAliasDeclaration qualifier def ->
                    addTypeAliasDeclaration qualifier def types

                _ ->
                    removeAnnotation types
    in
        List.foldl findType [] statements


addAnnotations : String -> List GeneratedType -> List GeneratedType
addAnnotations text types =
    let
        firstType =
            Maybe.withDefault emptyType (List.head types)

        addAnnotationsTo t =
            { t | annotations = t.annotations ++ findAnnotations text }
    in
        case firstType.declaration of
            Nothing ->
                (addAnnotationsTo firstType) :: Maybe.withDefault [] (List.tail types)

            Just declaration ->
                (addAnnotationsTo emptyType) :: types


addTypeDeclaration : Type -> List Type -> List GeneratedType -> List GeneratedType
addTypeDeclaration qualifier defs types =
    types


addTypeAliasDeclaration : Type -> Type -> List GeneratedType -> List GeneratedType
addTypeAliasDeclaration qualifier def types =
    types


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
