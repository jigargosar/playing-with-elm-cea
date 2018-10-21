port module Port exposing (log, wheel)

import Json.Decode as D
import Json.Encode as E


port wheel : (E.Value -> msg) -> Sub msg


port log : E.Value -> Cmd msg
