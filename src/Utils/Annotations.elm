module Utils.Annotations exposing (Annotation, findAnnotations)

import Regex exposing (..)


type alias Annotation =
    { name : String
    , params : List ( String, String )
    }


findAnnotations : String -> List Annotation
findAnnotations text =
    List.filterMap findAnnotation (String.split "\n" text)


findAnnotation : String -> Maybe Annotation
findAnnotation line =
    case find (AtMost 1) (regex ">\\s+([^:]+):\\s+") line of
        [ match ] ->
            Maybe.map (\name -> { name = name, params = matchParams match line }) (matchName match)

        _ ->
            Nothing


matchName : Match -> Maybe String
matchName match =
    List.head match.submatches
        |> Maybe.withDefault Nothing
        |> Maybe.map cleanName


matchParams : Match -> String -> List ( String, String )
matchParams match line =
    let
        paramsPart =
            String.dropLeft (match.index + String.length match.match) line

        nameAndContent submatches =
            case submatches of
                [ name, content ] ->
                    ( cleanName (Maybe.withDefault "" name), Maybe.withDefault "" content )

                _ ->
                    ( "", "" )
    in
        find All (regex "([^`]*)`([^`]+)`") paramsPart
            |> List.map (.submatches >> nameAndContent)


cleanName : String -> String
cleanName =
    String.toLower >> String.trim
