module Functions.Discover exposing (findFunctions, DiscoveredFunction)

import Ast.Statement exposing (..)
import Ast.Expression exposing (..)
import Utils.Annotations exposing (..)


type alias DiscoveredFunction =
    { annotations : List Annotation
    , functionType : Type
    , params : List Expression
    , body : Expression
    }


type alias PartialFunction =
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


findFunctions : List Statement -> List DiscoveredFunction
findFunctions statements =
    let
        findFunction stmt types =
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
        List.foldl findFunction [] statements
            |> List.filterMap completeFunction


completeFunction : PartialFunction -> Maybe DiscoveredFunction
completeFunction fn =
    Maybe.map3
        (\t p b -> { annotations = fn.annotations, functionType = t, params = p, body = b })
        fn.functionType
        fn.params
        fn.body


addAnnotations : String -> List PartialFunction -> List PartialFunction
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


addTypeDeclaration : String -> Type -> List PartialFunction -> List PartialFunction
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


addDeclaration : String -> List Expression -> Expression -> List PartialFunction -> List PartialFunction
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


firstFn : List PartialFunction -> PartialFunction
firstFn functions =
    Maybe.withDefault emptyFn (List.head functions)


emptyFn : PartialFunction
emptyFn =
    { annotations = [], name = Nothing, functionType = Nothing, params = Nothing, body = Nothing }


completeness : PartialFunction -> Completeness
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


removeAnnotation : List PartialFunction -> List PartialFunction
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
