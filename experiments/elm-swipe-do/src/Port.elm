port module Port exposing (cacheTodoC, focusSelector, logS, warn, wheel)

import Json.Decode as D
import Json.Encode as E


port wheel : (E.Value -> msg) -> Sub msg


port logS : String -> Cmd msg


port warn : List String -> Cmd msg


port cacheTodoC : E.Value -> Cmd msg


port focusSelector : String -> Cmd msg
