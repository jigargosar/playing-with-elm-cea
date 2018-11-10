module AddTodoDialog exposing (Model, Msg(..), OutMsg(..), getBackdropDomId, rootDomId, update)

import ContextStore exposing (ContextId)
import DomX exposing (DomId)
import Focus exposing (FocusResult)
import Log
import TodoStore exposing (TodoContent)
import UpdateReturn exposing (..)


type alias Model =
    { content : TodoContent
    , contextId : ContextId
    }


type OutMsg
    = ActionOut TodoContent ContextId
    | ClosedOut


type Msg
    = FocusResult FocusResult
    | BackdropClicked DomId
    | ContentChanged TodoContent
    | ContextIdChanged ContextId


rootDomId uid =
    "add-todo-dialog" ++ uid


getBackdropDomId uid =
    rootDomId uid ++ "-backdrop"


update : String -> Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update uniqueId message =
    (case message of
        FocusResult r ->
            addCmd (Log.focusResult "ContextPopup.elm" r)
                >> withNoOutMsg

        BackdropClicked targetId ->
            if targetId == getBackdropDomId uniqueId then
                withOutMsg (\_ -> ClosedOut)

            else
                withNoOutMsg

        ContentChanged content ->
            withNoOutMsg

        ContextIdChanged contextId ->
            withNoOutMsg
    )
        << pure
