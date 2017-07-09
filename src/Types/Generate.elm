module Types.Generate exposing (..)

import Utils.Interpolate exposing (..)
import Types.Refine exposing (..)


generateTypeConverters : Type -> String
generateTypeConverters t =
    case t of
        RecordType name fields ->
            generateRecordConverters name fields

        UnionType name options ->
            generateUnionConverters name options

        _ ->
            ""


generateRecordConverters : String -> List ( String, Type ) -> String
generateRecordConverters name fields =
    -- TODO
    interpolate """
        typeConverters.toElm{0} = value => {}
        typeConverters.toJs{0} = value => {}
        """
        [ name ]


generateUnionConverters : String -> List String -> String
generateUnionConverters name options =
    -- TODO
    interpolate """
        typeConverters.toElm{0} = value => {}
        typeConverters.toJs{0} = value => {}
        """
        [ name ]
