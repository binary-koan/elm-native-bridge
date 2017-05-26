module NativeBridge.Params exposing (..)

import NativeBridge.Fields exposing (FieldType)


type Param
    = Dynamic FieldType
    | FixedBool Bool
    | FixedInt Int
    | FixedFloat Float
    | FixedString String
    | FixedList (List Param)
    | FixedObject (List ( String, Param ))
    | FixedNull
    | FixedUndefined


param : FieldType -> Param
param t =
    Dynamic t


fixedBool : Bool -> Param
fixedBool b =
    FixedBool b


fixedInt : Int -> Param
fixedInt i =
    FixedInt i


fixedFloat : Float -> Param
fixedFloat f =
    FixedFloat f


fixedString : String -> Param
fixedString s =
    FixedString s


fixedList : List Param -> Param
fixedList list =
    FixedList list


fixedObject : List ( String, Param ) -> Param
fixedObject fields =
    FixedObject fields


fixedNull : Param
fixedNull =
    FixedNull


fixedUndefined : Param
fixedUndefined =
    FixedUndefined
