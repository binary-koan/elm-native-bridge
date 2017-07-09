module Main exposing (main)

import Task exposing (Task)
import Ast exposing (parseModule)
import Ast.BinOp exposing (operators)
import Ast.Statement exposing (Statement)
import Generation.Types exposing (..)


elmFile =
    """
module FS exposing (..)

{-| Wraps the native Node FS module

> Subject: `require('fs')`
> Elm Transformer: `Task` uses `callback`
> Elm Transformer: `Result` uses `exception`

-}

-- import Native.FS

import Task exposing (Task)


{-|

> Enum: `CanAccess` is `subject.constants.F_OK`
> Enum: `CanRead` is `subject.constants.R_OK`
> Enum: `CanWrite` is `subject.constants.W_OK`
> Enum: `CanExecute` is `subject.constants.X_OK`
> Enum Default: `CanRead`
-}
type FileAccess
    = CanAccess
    | CanRead
    | CanWrite
    | CanExecute


{-|

> Enum: `Read` is string `r`
> Enum: `ReadWriteSync` is string `rs+`
> Enum: `Write` is string `w`
> Enum: `WriteStrict` is string `wx`
> Enum: `ReadWrite` is string `w+`
> Enum: `ReadWriteStrict` is string `wx+`
> Enum: `Append` is string `a`
> Enum: `AppendStrict` is string `ax`
> Enum: `ReadAppend` is string `a+`
> Enum: `ReadAppendStrict` is string `ax+`
> Enum Default: `Read`
-}
type OpenFlag
    = Read
    | ReadWriteSync
    | Write
    | WriteStrict
    | ReadWrite
    | ReadWriteStrict
    | Append
    | AppendStrict
    | ReadAppend
    | ReadAppendStrict


type alias AppendOptions =
    { encoding : String
    , mode : Int
    , flag : OpenFlag
    }


type alias FileError =
    { message : Maybe String
    , code : Maybe String
    , syscall : Maybe String
    , path : Maybe String
    }


openMode : Int -> Int -> Int -> Int
openMode u g a =
    u * 64 + g * 8 + a


defaultAppendOptions : AppendOptions
defaultAppendOptions =
    { encoding = "utf8", mode = openMode 6 6 6, flag = Append }


{-|

> JS Transformer: `levels` uses `combineFlags`
-}
access : String -> List FileAccess -> Task FileError ()
access path levels =
    Native.FS.access path levels


{-|

> JS Transformer: `levels` uses `combineFlags`
-}
accessSync : String -> List FileAccess -> Result FileError ()
accessSync path levels =
    Native.FS.accessSync path levels


appendFile : String -> String -> Task FileError ()
appendFile path contents =
    Native.FS.appendFile path contents defaultAppendOptions


appendFileWithOptions : String -> String -> AppendOptions -> Task FileError ()
appendFileWithOptions path contents options =
    Native.FS.appendFile path contents options


appendFileSync : String -> String -> Result FileError ()
appendFileSync path contents =
    Native.FS.appendFileSync path contents defaultAppendOptions


appendFileSyncWithOptions : String -> String -> AppendOptions -> Result FileError ()
appendFileSyncWithOptions path contents options =
    Native.FS.appendFileSync path contents options


chmod : String -> Int -> Task FileError ()
chmod path mode =
    Native.FS.chmod path mode


chmodSync : String -> Int -> Result FileError ()
chmodSync path mode =
    Native.FS.chmodSync path mode


chown : String -> Int -> Int -> Task FileError ()
chown path uid gid =
    Native.FS.chown path uid gid


chownSync : String -> Int -> Int -> Result FileError ()
chownSync path uid gid =
    Native.FS.chownSync path uid gid


close : Int -> Task FileError ()
close fd =
    Native.FS.close fd


closeSync : Int -> Result FileError ()
closeSync fd =
    Native.FS.closeSync fd


existsSync : String -> Bool
existsSync path =
    Native.FS.existsSync path


fchmod : Int -> Int -> Task FileError ()
fchmod fd mode =
    Native.FS.fchmod fd mode


fchmodSync : Int -> Int -> Result FileError ()
fchmodSync fd mode =
    Native.FS.fchmodSync fd mode


fchown : Int -> Int -> Int -> Task FileError ()
fchown fd uid gid =
    Native.FS.fchown fd uid gid
"""


type alias GeneratedFile =
    { content : String
    , filename : String
    }


main : Program Never () ()
main =
    Platform.program
        { init =
            ( ()
            , Task.attempt
                (\_ -> ())
                (writeFiles (generateNative elmFile))
            )
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


generateNative : String -> Result String GeneratedFile
generateNative content =
    let
        ast =
            parseModule operators content

        generated statements =
            { content = generateModule statements
            , filename = "Native/FS.js"
            }
    in
        case ast of
            Err ( _, details, messages ) ->
                Err (formatError details.data details.position messages)

            Ok ( _, _, statements ) ->
                Ok (generated statements)


generateModule : List Statement -> String
generateModule statements =
    """
    var binary_koan$Native$FS = (() => {
        var typeConverters = {}, functions = {}
        var context = require('fs')

        return functions
    })()
    """


writeFiles : Result String GeneratedFile -> Task String String
writeFiles result =
    case result of
        Err err ->
            err
                |> Debug.log ("Error while parsing:\n" ++ err ++ "\n")
                |> Task.fail

        Ok file ->
            Debug.log (file.filename ++ "\n" ++ file.content ++ "\n") file.content
                |> Task.succeed


formatError : String -> Int -> List String -> String
formatError data position messages =
    let
        getLine position str =
            prevChars position str ++ getChar position str ++ nextChars position str

        prevChars position str =
            if position == 0 || getChar (position - 1) str == "\n" then
                ""
            else
                getChar (position - 1) str ++ prevChars (position - 1) str

        nextChars position str =
            if position == 0 || getChar (position + 1) str == "\n" then
                ""
            else
                getChar (position + 1) str ++ nextChars (position + 1) str

        getChar position str =
            String.slice position (position + 1) str
    in
        (String.join "\n" messages) ++ " @ " ++ (toString position) ++ ":\n  " ++ getLine position data
