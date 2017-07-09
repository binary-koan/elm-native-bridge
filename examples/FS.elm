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
    { code : Maybe String
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



-- fchownSync : Integer -> Integer -> Integer -> Result FileError ()
--     |> transformResult (resultFromException convertFileError pass)
-- fdatasync : Integer -> Task FileError ()
--     |> transformResult (taskFromCallback convertFileError pass)
-- fdatasyncSync : Integer -> Result FileError ()
--     |> transformResult (resultFromException convertFileError pass)
-- fstat : Integer -> Task FileError Stats
--     |> transformResult (taskFromCallback convertFileError statsToElm)
-- fstatSync : Integer -> Result FileError Stats
--     |> transformResult (resultFromException convertFileError statsToElm)
-- fsync : Integer -> Task FileError ()
--     |> transformResult (taskFromCallback convertFileError pass)
-- fsyncSync : Integer -> Result FileError ()
--     |> transformResult (resultFromException convertFileError pass)
-- ftruncate : Integer -> Integer -> Task FileError ()
--     |> transformResult (taskFromCallback convertFileError pass)
-- ftruncateSync : Integer -> Integer -> Result FileError ()
--     |> transformResult (resultFromException convertFileError pass)
-- futimes : Integer -> Integer -> Integer -> Task FileError ()
--     |> transformResult (taskFromCallback convertFileError pass)
-- futimesSync : Integer -> Integer -> Integer -> Result FileError ()
--     |> transformResult (resultFromException convertFileError pass)
-- lchmod : String -> Task FileError ()
--     |> transformResult (taskFromCallback convertFileError pass)
-- lchmodSync : String -> Result FileError ()
--     |> transformResult (resultFromException convertFileError pass)
-- lchown : String -> Integer -> Task FileError ()
--     |> transformResult (taskFromCallback convertFileError pass)
-- lchownSync : String -> Integer -> Result FileError ()
--     |> transformResult (resultFromException convertFileError pass)
-- link : String -> String -> Task FileError ()
--     |> transformResult (taskFromCallback convertFileError pass)
-- linkSync : String -> String -> Result FileError ()
--     |> transformResult (resultFromException convertFileError pass)
-- lstat : String -> Task FileError Stats
--     |> transformResult (taskFromCallback convertFileError statsToElm)
-- lstatSync : String -> Result FileError Stats
--     |> transformResult (resultFromException convertFileError statsToElm)
-- mkdir : String -> Task FileError ()
--     |> transformResult (taskFromCallback convertFileError pass)
-- mkdirSync : String -> Result FileError ()
--     |> transformResult (resultFromException convertFileError pass)
-- mkdir_ : String -> Integer -> Task FileError ()
--     |> transformResult (taskFromCallback convertFileError pass)
--     |> jsFunction "mkdir"
-- mkdirSync_ : String -> Integer -> Result FileError ()
--     |> transformResult (resultFromException convertFileError pass)
--     |> jsFunction "mkdirSync"
-- mkdtemp : String -> Task FileError String
--     |> transformResult (taskFromCallback convertFileError pass)
-- mkdtempSync : String -> Result FileError String
--     |> transformResult (resultFromException convertFileError pass)
-- mkdtemp_ : String -> String -> Task FileError String
--     |> transformResult (taskFromCallback convertFileError pass)
--     |> jsFunction "mkdtemp"
-- mkdtempSync_ : String -> String -> Result FileError String
--     |> transformResult (resultFromException convertFileError pass)
--     |> jsFunction "mkdtempSync"
