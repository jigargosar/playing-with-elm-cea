port module Port exposing (log, logR, logS, wheel)

import Json.Decode as D
import Json.Encode as E


port wheel : (E.Value -> msg) -> Sub msg


port log : E.Value -> Cmd msg


port logS : String -> Cmd msg


port logR : { err : String } -> Cmd msg
