port module Port exposing (cacheTodoC, focus, logS, wheel)

import Json.Decode as D
import Json.Encode as E


port wheel : (E.Value -> msg) -> Sub msg


port logS : String -> Cmd msg


port cacheTodoC : E.Value -> Cmd msg


port focus : String -> Cmd msg
