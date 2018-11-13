module CmdDialog exposing (Model, Msg(..), OutMsg(..), init, update, view)

import Html.Styled exposing (text)
import UI exposing (sDiv)
import UpdateReturn exposing (..)


type alias Model =
    {}


init =
    {}


type Msg
    = NoOp


type OutMsg
    = Out1


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update message =
    (case message of
        --        FocusResult r ->
        --            addCmd (Log.focusResult "ContextPopup.elm" r)
        --                >> withNoOutMsg
        NoOp ->
            withNoOutMsg
    )
        << pure


view model =
    sDiv [] [] [ text "foo" ]
