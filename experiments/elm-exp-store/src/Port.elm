port module Port exposing
    ( PopperStyles
    , activeElementsParentIdList
    , cacheContextStore
    , cacheTodoStore
    , createPopper
    , destroyPopper
    , documentFocusChanged
    , focusSelector
    , popperStylesChanged
    , popperStylesSet
    , warn
    , wheel
    )

import DomX exposing (DomId)
import Json.Decode as D
import Json.Encode as E


port wheel : (E.Value -> msg) -> Sub msg


port warn : List String -> Cmd msg


port cacheTodoStore : E.Value -> Cmd msg


port cacheContextStore : E.Value -> Cmd msg


port focusSelector : String -> Cmd msg


port activeElementsParentIdList : (List String -> msg) -> Sub msg


port documentFocusChanged : (Bool -> msg) -> Sub msg


type alias PopperStyles =
    { --      styles : { position : String, left : Int, top : Int, transform : String, willChange : String }
      styles : List ( String, String )
    , attributes : List ( String, String )
    }


port popperStylesSet : (PopperStyles -> msg) -> Sub msg


port popperStylesChanged : (PopperStyles -> msg) -> Sub msg


type alias RefDomId =
    DomId


type alias PopperDomId =
    DomId


port createPopper : ( RefDomId, PopperDomId ) -> Cmd msg


port destroyPopper : ( RefDomId, PopperDomId ) -> Cmd msg
