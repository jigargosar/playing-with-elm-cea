port module Port exposing
    ( activeElementsParentIdList
    , cacheContextStore
    , cacheTodoStore
    , focusSelector
    , warn
    , wheel
    , windowFocusChanged
    )

import Json.Decode as D
import Json.Encode as E


port wheel : (E.Value -> msg) -> Sub msg


port warn : List String -> Cmd msg


port cacheTodoStore : E.Value -> Cmd msg


port cacheContextStore : E.Value -> Cmd msg


port focusSelector : String -> Cmd msg


port activeElementsParentIdList : (List String -> msg) -> Sub msg


port windowFocusChanged : (Bool -> msg) -> Sub msg
