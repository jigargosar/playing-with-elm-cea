port module Port exposing (log, logS, wheel)

import Json.Decode as D
import Json.Encode as E


port wheel : (E.Value -> msg) -> Sub msg


port log : E.Value -> Cmd msg


port logS : String -> Cmd msg
