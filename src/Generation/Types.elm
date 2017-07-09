module Generation.Types exposing (findTypes)

import Ast.Statement exposing (..)
import Generation.Annotations exposing (..)


type alias GeneratedType =
    { annotations : List Annotation
    , declaration : Maybe Statement
    }


findTypes : List Statement -> Result String (List GeneratedType)
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
                    Ok (removeAnnotation types)
    in
        List.foldl (\stmt types -> Result.andThen (findType stmt) types) (Ok []) statements


addAnnotations : String -> List GeneratedType -> Result String (List GeneratedType)
addAnnotations text types =
    let
        firstType =
            Maybe.withDefault emptyType (List.head types)

        addAnnotationsTo t =
            { t | annotations = t.annotations ++ findAnnotations text }
    in
        case firstType.declaration of
            Nothing ->
                Ok <| (addAnnotationsTo firstType) :: Maybe.withDefault [] (List.tail types)

            Just declaration ->
                Ok <| (addAnnotationsTo emptyType) :: types


addTypeDeclaration : Type -> List Type -> List GeneratedType -> Result String (List GeneratedType)
addTypeDeclaration qualifier defs types =
    Ok types


addTypeAliasDeclaration : Type -> Type -> List GeneratedType -> Result String (List GeneratedType)
addTypeAliasDeclaration qualifier def types =
    Ok types


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
