module CreateTodoDialog exposing (Model, Msg, OutMsg(..), autoFocus, init, update, view)

import BasicsX exposing (defaultEmptyStringTo)
import ContextStore exposing (Context, ContextId, ContextStore)
import Css
import CssAtoms exposing (..)
import DomX exposing (DomId, onClickTargetId)
import Focus exposing (FocusResult)
import HotKey
import Html.Styled exposing (Html, div, input, option, select, styled, text)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput)
import Log
import Styles exposing (..)
import TodoStore exposing (TodoContent)
import UI exposing (sDiv)
import UpdateReturn exposing (..)


type alias Model =
    { content : TodoContent
    , contextId : ContextId
    }


init contextId =
    { content = "", contextId = contextId }


autoFocus =
    AutoFocus


type OutMsg
    = Submit TodoContent ContextId
    | Cancel


type Msg
    = FocusResult FocusResult
    | BackdropClicked DomId
    | ContentChanged TodoContent
    | ContextIdChanged ContextId
    | ContentInputKeyDown HotKey.Event
    | ContextIdSelectKeyDown HotKey.Event
    | AutoFocus


rootDomId uid =
    "add-todo-dialog" ++ uid


withSubmitOutMsg : ( Model, Cmd Msg ) -> ( Model, Cmd Msg, Maybe OutMsg )
withSubmitOutMsg =
    withOutMsg
        (\{ content, contextId } ->
            if String.isEmpty content then
                Cancel

            else
                Submit content contextId
        )


withCancelOutMsg : ( Model, Cmd Msg ) -> ( Model, Cmd Msg, Maybe OutMsg )
withCancelOutMsg =
    withOutMsg (always Cancel)


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update message =
    (case message of
        AutoFocus ->
            addCmd (Focus.attempt FocusResult inputId)
                >> withNoOutMsg

        FocusResult r ->
            addCmd (Log.focusResult "ContextPopup.elm" r)
                >> withNoOutMsg

        BackdropClicked targetId ->
            if targetId == backdropId then
                withSubmitOutMsg

            else
                withNoOutMsg

        ContentChanged content ->
            mapModel (\model -> { model | content = content })
                >> withNoOutMsg

        ContextIdChanged contextId ->
            withNoOutMsg

        ContentInputKeyDown ke ->
            case ke of
                ( [], "Enter" ) ->
                    withSubmitOutMsg

                ( [], "Escape" ) ->
                    withCancelOutMsg

                _ ->
                    withNoOutMsg

        ContextIdSelectKeyDown ke ->
            case ke of
                ( [], "Enter" ) ->
                    withSubmitOutMsg

                ( [], "Escape" ) ->
                    withCancelOutMsg

                _ ->
                    withNoOutMsg
    )
        << pure


backdropId =
    "add-todo-modal-backdrop"


inputId =
    "add-todo-modal-content-input"


view : ContextStore -> Model -> Html Msg
view contextStore model =
    let
        contexts =
            ContextStore.list contextStore

        viewInboxOption =
            styled option [] [ value ContextStore.defaultId ] [ text <| ContextStore.defaultName ]

        viewContextOption context =
            styled option [] [ value context.id ] [ text <| context.name ]
    in
    UI.backdrop
        [ id backdropId
        , onClickTargetId BackdropClicked
        ]
        [ div
            [ class "bg-white br4 shadow-1 pa3 measure w-100"
            ]
            [ sDiv [ vs, w100, rowCY ]
                []
                [ input
                    [ id inputId
                    , placeholder "Task Content"
                    , class "flex-auto pa3"
                    , value model.content
                    , onInput ContentChanged
                    , HotKey.onKeyDown ContentInputKeyDown
                    ]
                    []
                ]
            , sDiv [ w100, rowCY, vs ]
                []
                [ styled select
                    [ fa
                    , Css.property "-webkit-appearance" "none"
                    ]
                    [ class "pa3"
                    , value model.contextId
                    , onInput ContextIdChanged
                    , HotKey.onKeyDown ContextIdSelectKeyDown
                    ]
                    (viewInboxOption :: List.map viewContextOption contexts)
                ]
            ]
        ]
