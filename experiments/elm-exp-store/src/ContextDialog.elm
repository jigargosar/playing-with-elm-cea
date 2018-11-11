module ContextDialog exposing (DialogMode(..), Model, Msg, OutMsg(..), autoFocus, initCreate, initEdit, update, view)

import BasicsX exposing (defaultEmptyStringTo)
import ContextStore exposing (Context, ContextId, ContextName, ContextStore)
import Css
import DomX exposing (DomId, onClickTargetId)
import Focus exposing (FocusResult)
import HotKey
import Html.Styled exposing (Html, div, input, option, select, styled, text)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onInput)
import Log
import Styles exposing (..)
import TodoStore exposing (Todo, TodoContent)
import UI exposing (sDiv)
import UpdateReturn exposing (..)


type DialogMode
    = Create
    | Edit Context


type alias Model =
    { name : ContextName
    , dialogMode : DialogMode
    }


initEdit : Context -> Model
initEdit context =
    { name = context.name, dialogMode = Edit context }


initCreate : Model
initCreate =
    { name = "", dialogMode = Create }


autoFocus =
    AutoFocus


type OutMsg
    = Submit DialogMode ContextName
    | Cancel


type Msg
    = FocusResult FocusResult
    | BackdropClicked DomId
    | NameChanged ContextName
    | NameInputKeyDown HotKey.Event
    | AutoFocus


withSubmitOutMsg : ( Model, Cmd Msg ) -> ( Model, Cmd Msg, Maybe OutMsg )
withSubmitOutMsg =
    withOutMsg
        (\{ dialogMode, name } ->
            if String.isEmpty name then
                Cancel

            else
                Submit dialogMode name
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
            addCmd (Log.focusResult "ContextDialog.elm" r)
                >> withNoOutMsg

        BackdropClicked targetId ->
            if targetId == backdropId then
                withSubmitOutMsg

            else
                withNoOutMsg

        NameChanged name ->
            mapModel (\model -> { model | name = name })
                >> withNoOutMsg

        NameInputKeyDown ke ->
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
    "context-dialog-backdrop"


inputId =
    "context-dialog-name-input"


view : Model -> Html Msg
view model =
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
                    , placeholder "Context Name"
                    , class "flex-auto pa3"
                    , value model.name
                    , onInput NameChanged
                    , HotKey.onKeyDown NameInputKeyDown
                    ]
                    []
                ]
            ]
        ]
