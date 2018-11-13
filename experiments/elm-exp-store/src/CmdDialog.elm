module CmdDialog exposing (Model, Msg(..), OutMsg(..), init, update, view)

import DomX exposing (DomId)
import Html.Styled exposing (div, text)
import Html.Styled.Events exposing (onClick)
import UI exposing (sDiv)
import UpdateReturn exposing (..)


type alias Model =
    {}


init =
    {}


type Msg
    = BackDropClicked DomId
    | AutoFocus


type OutMsg
    = Cancel


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update message =
    (case message of
        AutoFocus ->
            withNoOutMsg

        --        FocusResult r ->
        --            addCmd (Log.focusResult "ContextPopup.elm" r)
        --                >> withNoOutMsg
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


view model =
    div [] [ UI.backdrop [ DomX.onClickTargetId BackDropClicked ] [] ]
