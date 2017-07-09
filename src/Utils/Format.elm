module Utils.Format exposing (format)

import Regex exposing (..)


format : String -> List String -> String
format string args =
    deindent string
        |> formatArg 0 args


deindent : String -> String
deindent string =
    let
        trimmed =
            string |> replace (AtMost 1) (regex "^\\n") (always "") |> String.trimRight

        firstIndent =
            find (AtMost 1) (regex "^\\s+") trimmed
                |> List.head

        replaceInLine indent line =
            replace All (regex ("^" ++ indent)) (always "") line
    in
        case firstIndent of
            Nothing ->
                trimmed

            Just match ->
                String.split "\n" trimmed
                    |> List.map (replaceInLine match.match)
                    |> String.join "\n"


formatArg : Int -> List String -> String -> String
formatArg index args string =
    case List.head args of
        Just value ->
            replaceAll ("{" ++ toString index ++ "}") value string
                |> formatArg (index + 1) (Maybe.withDefault [] (List.tail args))

        Nothing ->
            string


replaceAll : String -> String -> String -> String
replaceAll old new string =
    string
        |> String.split old
        |> String.join new
