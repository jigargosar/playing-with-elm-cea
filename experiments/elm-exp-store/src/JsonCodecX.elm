module JsonCodecX exposing (decodeValue)

import Json.Decode as D exposing (errorToString)
import Json.Encode as E exposing (Value)
import JsonCodec as JC exposing (Codec)
import Log


decodeValue : Codec a -> a -> Value -> ( Maybe Log.Line, a )
decodeValue codec defaultAnswer =
    D.decodeValue (JC.decoder codec)
        >> (\result ->
                case result of
                    Ok answer ->
                        ( Nothing, answer )

                    Err error ->
                        ( Just [ errorToString error ], defaultAnswer )
           )
