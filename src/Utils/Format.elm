module Utils.Format exposing (format)

import Regex exposing (..)


format : String -> List String -> String
format string args =
    deindent string
        |> formatArg 0 args


formatArg : Int -> List String -> String -> String
formatArg index args string =
    case List.head args of
        Just value ->
            replaceWithIndent ("{" ++ toString index ++ "}") value string
                |> formatArg (index + 1) (Maybe.withDefault [] (List.tail args))

        Nothing ->
            string


replaceWithIndent : String -> String -> String -> String
replaceWithIndent old new string =
    string
        |> String.split old
        |> joinWithIndent new


joinWithIndent : String -> List String -> String
joinWithIndent joiner parts =
    case parts of
        [] ->
            ""

        [ end ] ->
            end

        first :: others ->
            first ++ indent (lastLineIndent first) joiner ++ (joinWithIndent joiner others)


indent : String -> String -> String
indent amount str =
    String.split "\n" str
        |> String.join ("\n" ++ amount)


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


lastLineIndent : String -> String
lastLineIndent str =
    let
        lastLine =
            String.split "\n" str |> List.reverse |> List.head |> Maybe.withDefault ""

        match =
            find (AtMost 1) (regex "^([ \t]+).*$") lastLine
                |> List.head
    in
        case match of
            Nothing ->
                ""

            Just match ->
                List.head match.submatches |> Maybe.withDefault Nothing |> Maybe.withDefault ""
