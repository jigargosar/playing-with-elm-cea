module CmdDialog exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Css exposing (absolute, position)
import DomX exposing (DomId)
import Focus
import HotKey
import Html.Styled exposing (Html, div, input, text)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick, onInput)
import Log
import Styles exposing (..)
import UI exposing (..)
import UpdateReturn exposing (..)


type alias Model =
    {}


init =
    {}


type Msg
    = BackDropClicked DomId
    | AutoFocus
    | FocusResult Focus.FocusResult


type OutMsg
    = Cancel


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update message =
    (case message of
        AutoFocus ->
            addEffect (getInputId >> Focus.attempt FocusResult)
                >> withNoOutMsg

        FocusResult r ->
            addCmd (Log.focusResult "CmdDialog.elm" r)
                >> withNoOutMsg

        BackDropClicked targetId ->
            withMaybeOutMsg
                (\model ->
                    if targetId == getBackdropDomId model then
                        Just Cancel

                    else
                        Nothing
                )
    )
        << pure


getDomIdPrefix model =
    "cmd-dialog"


getBackdropDomId =
    getDomIdPrefix >> (++) "-backdrop"


getInputId =
    getDomIdPrefix >> (++) "-cmd-input"


view : Model -> Html Msg
view model =
    div []
        [ UI.backdrop [ id <| getBackdropDomId model, DomX.onClickTargetId BackDropClicked ]
            [ sDiv []
                [ class "bg-white br4 shadow-1 pa3 measure w-100"
                ]
                [ sDiv [ vs, w100, rowCY ]
                    []
                    [ input
                        [ id <| getInputId model
                        , placeholder "Task Content"
                        , class "flex-auto pa3"
                        , value "model.content"

                        --                          , onInput ContentChanged
                        --                          , HotKey.onKeyDown ContentInputKeyDown
                        ]
                        []
                    ]
                ]
            ]
        ]
