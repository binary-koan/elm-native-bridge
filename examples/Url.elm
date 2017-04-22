import NativeBridge exposing (..)
import NativeBridge.Fields exposing (..)
import NativeBridge.Params exposing (..)


main : BridgeGenerator
main =
    generate "Url" "require('url')"
        [ recordType "Url"
            [ field "auth" (maybe string)
            , field "hash" (maybe string)
            , field "host" (maybe string)
            , field "hostname" (maybe string)
            , field "href" string
            , field "path" (maybe string)
            , field "pathname" (maybe string)
            , field "port" (maybe string)
            , field "protocol" (maybe string)
            , field "query" (dict string)
            , field "search" (maybe string)
            , field "slashes" bool
            ]
        ]
        [ function "format" [ "Url", "String" ]
        , function "parse" [ "String", "Url" ]
            |> appendParams [ fixedBool True ]
        ]
