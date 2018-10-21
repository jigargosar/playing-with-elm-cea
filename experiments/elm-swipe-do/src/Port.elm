port module Port exposing (logS, wheel)

import Json.Decode as D
import Json.Encode as E


port wheel : (E.Value -> msg) -> Sub msg


port logS : String -> Cmd msg
