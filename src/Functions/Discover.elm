module Functions.Discover exposing (discoverFunctions)

import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import Generation.Annotations exposing (..)


type alias GeneratedFunction =
    { annotations : List Annotation
    , name : Maybe String
    , functionType : Maybe Type
    , params : Maybe (List Expression)
    , body : Maybe Expression
    }


type Completeness
    = Empty
    | NoType
    | NoDeclaration
    | Complete


discoverFunctions : List Statement -> List GeneratedFunction
discoverFunctions statements =
    let
        findType stmt types =
            case stmt of
                Comment text ->
                    addAnnotations text types

                FunctionTypeDeclaration name declaration ->
                    addTypeDeclaration name declaration types

                FunctionDeclaration name params body ->
                    addDeclaration name params body types

                _ ->
                    removeAnnotation types
    in
        List.foldl findType [] statements


addAnnotations : String -> List GeneratedFunction -> List GeneratedFunction
addAnnotations text functions =
    let
        first =
            firstFn functions

        addAnnotationsTo t =
            { t | annotations = t.annotations ++ findAnnotations text }
    in
        case completeness (firstFn functions) of
            Complete ->
                (addAnnotationsTo emptyFn) :: functions

            _ ->
                (addAnnotationsTo first) :: Maybe.withDefault [] (List.tail functions)


addTypeDeclaration : String -> Type -> List GeneratedFunction -> List GeneratedFunction
addTypeDeclaration name t functions =
    let
        first =
            firstFn functions
    in
        case completeness first of
            Complete ->
                { emptyFn | name = Just name, functionType = Just t } :: functions

            NoType ->
                { first | name = Just name, functionType = Just t } :: functions

            _ ->
                { emptyFn | name = Just name, functionType = Just t } :: Maybe.withDefault [] (List.tail functions)


addDeclaration : String -> List Expression -> Expression -> List GeneratedFunction -> List GeneratedFunction
addDeclaration name params body functions =
    let
        first =
            firstFn functions
    in
        case ( completeness first, first.name == Just name ) of
            ( NoDeclaration, True ) ->
                { first | params = Just params, body = Just body } :: functions

            ( Complete, _ ) ->
                -- Can't handle a function with no type declaration
                functions

            ( _, _ ) ->
                -- Declaration must immediately follow type definition
                Maybe.withDefault [] (List.tail functions)


firstFn : List GeneratedFunction -> GeneratedFunction
firstFn functions =
    Maybe.withDefault emptyFn (List.head functions)


emptyFn : GeneratedFunction
emptyFn =
    { annotations = [], name = Nothing, functionType = Nothing, params = Nothing, body = Nothing }


completeness : GeneratedFunction -> Completeness
completeness fn =
    case ( fn.annotations, fn.functionType, fn.body ) of
        ( [], Nothing, Nothing ) ->
            Empty

        ( _, Nothing, Nothing ) ->
            NoType

        ( _, _, Nothing ) ->
            NoDeclaration

        _ ->
            Complete


removeAnnotation : List GeneratedFunction -> List GeneratedFunction
removeAnnotation functions =
    let
        firstFn =
            Maybe.withDefault emptyFn (List.head functions)
    in
        case firstFn.annotations of
            [] ->
                functions

            _ ->
                Maybe.withDefault [] (List.tail functions)
