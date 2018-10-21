port module Port exposing (wheel)

import Json.Decode as D
import Json.Encode as E


port wheel : (E.Value -> msg) -> Sub msg
