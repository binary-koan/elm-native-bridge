module NativeBridge.Fields exposing (..)


type FieldType
    = BoolField
    | IntField
    | FloatField
    | StringField
    | ListField FieldType
    | MaybeField FieldType
    | DictField FieldType
    | GeneratedField String


type alias Field =
    { name : String
    , fieldType : FieldType
    }


field : String -> FieldType -> Field
field name fieldType =
    { name = name, fieldType = fieldType }


bool : FieldType
bool =
    BoolField


int : FieldType
int =
    IntField


float : FieldType
float =
    FloatField


string : FieldType
string =
    StringField


list : FieldType -> FieldType
list t =
    ListField t


maybe : FieldType -> FieldType
maybe t =
    MaybeField t


dict : FieldType -> FieldType
dict t =
    DictField t


generated : String -> FieldType
generated name =
    GeneratedField name
