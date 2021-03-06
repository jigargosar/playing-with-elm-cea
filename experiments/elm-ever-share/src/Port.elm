port module Port exposing (cacheTodoStore, focusSelector, warn, wheel)

import Json.Decode as D
import Json.Encode as E


port wheel : (E.Value -> msg) -> Sub msg


port warn : List String -> Cmd msg


port cacheTodoStore : E.Value -> Cmd msg


port focusSelector : String -> Cmd msg
