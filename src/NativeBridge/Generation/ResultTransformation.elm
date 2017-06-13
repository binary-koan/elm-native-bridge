module NativeBridge.Generation.ResultTransformation exposing (..)

import Task
import String.Interpolate exposing (..)
import NativeBridge.Types exposing (..)
import NativeBridge.Generation.TypeConversion exposing (..)


transformer2 : (BridgeType -> BridgeType -> BridgeGenerator -> TransformerResult) -> Transformer
transformer2 fn =
    \options generator ->
        case options.typeParams of
            [ leftType, rightType ] ->
                fn leftType rightType generator

            _ ->
                Task.fail "Expected your '" ++ options.typeName ++ "' transformer to match a type with 2 parameters"


callbackTaskTransformer : Transformer
callbackTaskTransformer =
    transformer2
        (\failType successType generator ->
            { returnValueWrapper =
                \original ->
                    interpolate """
                    _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
                        return {0}
                    })
                    """ [ original ]
            , extraParams =
                interpolate """
                function(err, result) {
                    if (err) {
                        return {0}
                    } else {
                        return {1}
                    }
                """ [ jsToElmConverter "err" failType generator, jsToElmConverter "result" successType generator ]
            }
        )
