module Generation.Annotations exposing (findAnnotations)

import Regex exposing (..)


type alias Annotation =
    { name : String
    , params : List String
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


matchParams : Match -> String -> List String
matchParams match line =
    let
        paramsPart =
            String.dropLeft (match.index + String.length match.match) line

        firstSubmatch submatches =
            List.head submatches |> Maybe.withDefault Nothing |> Maybe.withDefault ""
    in
        find All (regex "`([^`]+)`") line
            |> List.map (.submatches >> firstSubmatch)
