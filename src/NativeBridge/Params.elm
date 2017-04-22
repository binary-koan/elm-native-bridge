module NativeBridge.Params exposing (..)


type FixedParam
    = FixedBool Bool
    | FixedInt Int
    | FixedFloat Float
    | FixedString String
    | FixedList (List FixedParam)
    | FixedObject (List ( String, FixedParam ))
    | FixedNull
    | FixedUndefined


fixedBool : Bool -> FixedParam
fixedBool b =
    FixedBool b


fixedInt : Int -> FixedParam
fixedInt i =
    FixedInt i


fixedFloat : Float -> FixedParam
fixedFloat f =
    FixedFloat f


fixedString : String -> FixedParam
fixedString s =
    FixedString s


fixedList : List FixedParam -> FixedParam
fixedList list =
    FixedList list


fixedObject : List ( String, FixedParam ) -> FixedParam
fixedObject fields =
    FixedObject fields


fixedNull : FixedParam
fixedNull =
    FixedNull


fixedUndefined : FixedParam
fixedUndefined =
    FixedUndefined
