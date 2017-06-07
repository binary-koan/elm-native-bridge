module Main exposing (..)

import NativeBridge.Generator exposing (..)
import NativeBridge.Fields exposing (..)
import NativeBridge.Params exposing (..)


main : RunGenerator
main =
    run generator


generator : BridgeGenerator
generator =
    generator "FS" "require('fs')"
        |> withTypes
            [ union "FileAccess"
                [ ( "CanAccess", js "$subject.constants.F_OK" )
                , ( "CanRead", js "$subject.constants.R_OK" )
                , ( "CanWrite", js "$subject.constants.W_OK" )
                , ( "CanExecute", js "$subject.constants.X_OK" )
                ]
            , union "OpenFlag"
                [ ( "Read", jsString "r" )
                , ( "ReadWriteSync", jsString "rs+" )
                , ( "Write", jsString "w" )
                , ( "WriteStrict", jsString "wx" )
                , ( "ReadWrite", jsString "w+" )
                , ( "ReadWriteStrict", jsString "wx+" )
                , ( "Append", jsString "a" )
                , ( "AppendStrict", jsString "ax" )
                , ( "ReadAppend", jsString "a+" )
                , ( "ReadAppendStrict", jsString "ax+" )
                ]
            , record "Permissions"
                [ "user : Int"
                , "group : Int"
                , "all : Int"
                ]
                |> toJs "$value.user * 64 + $value.group * 8 + $value.all"
            ]
        |> withElm
            """
            permissions : Int -> Int -> Int -> Permissions
            permissions u g a =
                { user = u, group = g, all = a }
            """
        |> withFunctions
            --TODO fs.read, fs.readSync, fs.watch, fs.watchFile, fs.unwatchFile
            (callbackSyncPair "access" "String -> List FileAccess" "()"
                ++ pairWithOptions "appendFile" "String -> String" "{ encoding : String, mode : Permissions, flag : OpenFlag }" "()"
                ++ callbackSyncPair "chmod" "String -> Permissions" "()"
                ++ callbackSyncPair "chown" "String -> Int -> Int" "()"
                ++ callbackSyncPair "close" "Int" "()"
                ++ [ function "existsSync : String -> Bool" ]
                ++ callbackSyncPair "fchmod" "Int -> Permissions" "()"
                ++ callbackSyncPair "fchown" "Int -> Int -> Int" "()"
                ++ callbackSyncPair "fdatasync" "Int" "()"
                ++ callbackSyncPair "fstat" "Int" "Stats"
                ++ callbackSyncPair "fsync" "Int" "()"
                ++ callbackSyncPair "ftruncate" "Int -> Int" "()"
                ++ callbackSyncPair "futimes" "Int -> Int -> Int" "()"
                ++ callbackSyncPair "lchmod : String -> Task FileError ()"
                ++ callbackSyncPair "lchown : String -> Int -> Task FileError ()"
                ++ callbackSyncPair "link : String -> String -> Task FileError ()"
                ++ callbackSyncPair "lstat : String -> Task FileError Stats"
                ++ pairWithOptions "mkdir" "String" "Permissions" "()"
                ++ pairWithOptions "mkdtemp" "String" "String" "String"
                ++ pairWithOptions "open" "String -> OpenFlag" "Permissions" "Int"
                ++ pairWithOptions "readdir" "String" "String" "List String"
                ++ pairWithOptions "readFile" "String" "{ encoding : String, flag : OpenFlag }" "String"
                ++ pairWithOptions "readlink" "String" "String" "String"
                ++ pairWithOptions "realpath" "String" "String" "String"
                ++ callbackSyncPair "rename" "String -> String" "()"
                ++ callbackSyncPair "rmdir" "String" "()"
                ++ callbackSyncPair "stat" "String" "Stats"
                ++ pairWithOptions "symlink" "String -> String" "String" "()"
                ++ callbackSyncPair "truncate" "String -> Int" "()"
                ++ callbackSyncPair "unlink" "String" "()"
                ++ callbackSyncPair "utimes" "String -> Int -> Int" "()"
                ++ pairWithOptions "write" "Int -> String" "Int -> String" "Int"
                ++ pairWithOptions "writeFile" "String -> String" "{ encoding : String, mode : Permissions, flag : OpenFlag }" "()"
                ++ methods "Stats"
                    [ "isFile : Bool"
                    , "isDirectory : Bool"
                    , "isBlockDevice : Bool"
                    , "isCharacterDevice : Bool"
                    , "isSymbolicLink : Bool"
                    , "isFIFO : Bool"
                    , "isSocket : Bool"
                    , "dev : Int"
                    , "ino : Int"
                    , "mode : Int"
                    , "nlink : Int"
                    , "uid : Int"
                    , "gid : Int"
                    , "rdev : Int"
                    , "size : Int"
                    , "blksize : Int"
                    , "blocks : Int"
                    , "atime : Time"
                    , "mtime : Time"
                    , "ctime : Time"
                    , "birthtime : Time"
                    ]
            )
        |> defaultTaskTransformer callbackTransformer
        |> defaultResultTransformer exceptionTransformer


callbackSyncPair : String -> String -> String -> List FunctionDefinition
callbackSyncPair name args result =
    [ function (name ++ " : " ++ args ++ " -> Task FileError " ++ result)
    , function (name ++ "Sync : " ++ args ++ " -> Result FileError " ++ result)
    ]


pairWithOptions : String -> String -> String -> String -> List FunctionDefinition
pairWithOptions name args optionArg result =
    let
        plainPair =
            callbackSyncPair name args result

        optionsPair =
            List.map2 (|>)
                (callbackSyncPair (name ++ "WithOptions") (args ++ " -> " ++ optionArg) result)
                [ jsFunction name, jsFunction (name ++ "Sync") ]
    in
        plainPair ++ optionsPair
