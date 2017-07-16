module Utils.Result exposing (..)


ensureAll : (x -> Result y z) -> List x -> Result y (List z)
ensureAll fn list =
    case List.head list of
        Nothing ->
            Ok []

        Just x ->
            Result.map2
                (\one others -> one :: others)
                (fn x)
                (ensureAll fn (Maybe.withDefault [] (List.tail list)))


errToMaybe : Result x y -> Maybe x
errToMaybe result =
    case result of
        Err err ->
            Just err

        Ok _ ->
            Nothing
