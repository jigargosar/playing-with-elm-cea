module AddTodoDialog exposing (Model, Msg(..), OutMsg(..), getBackdropDomId, rootDomId, update)

import ContextStore exposing (ContextId)
import DomX exposing (DomId, onClickTargetId)
import Focus exposing (FocusResult)
import HotKey
import Html.Styled exposing (Html, div, input)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput)
import Log
import TodoStore exposing (TodoContent)
import UI
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
    | ContentInputKeyDown HotKey.Event


rootDomId uid =
    "add-todo-dialog" ++ uid


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update message =
    (case message of
        FocusResult r ->
            addCmd (Log.focusResult "ContextPopup.elm" r)
                >> withNoOutMsg

        BackdropClicked targetId ->
            if targetId == backdropId then
                withOutMsg (\_ -> ClosedOut)

            else
                withNoOutMsg

        ContentChanged content ->
            withNoOutMsg

        ContextIdChanged contextId ->
            withNoOutMsg

        ContentInputKeyDown ke ->
            withNoOutMsg
    )
        << pure


backdropId =
    "add-todo-modal-backdrop"


view : DomId -> String -> Html Msg
view inputId content =
    UI.backdrop
        [ id backdropId
        , onClickTargetId BackdropClicked
        ]
        [ div
            [ class "bg-white br4 shadow-1 pa3 measure w-100"
            ]
            [ div [ class "w-100 flex" ]
                [ input
                    [ id inputId
                    , class "flex-auto pa3"
                    , value content
                    , onInput ContentChanged
                    , HotKey.onKeyDown ContentInputKeyDown
                    ]
                    []
                ]
            ]
        ]
