module Utils.Interpolate exposing (interpolate)


interpolate : String -> List String -> String
interpolate string args =
    interpolateArg 0 args string


interpolateArg : Int -> List String -> String -> String
interpolateArg index args string =
    case List.head args of
        Just value ->
            replaceAll ("{" ++ toString index ++ "}") value string
                |> interpolateArg (index + 1) (Maybe.withDefault [] (List.tail args))

        Nothing ->
            string


replaceAll : String -> String -> String -> String
replaceAll old new string =
    string
        |> String.split old
        |> String.join new
