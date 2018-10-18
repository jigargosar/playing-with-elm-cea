module Pages.Notes exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Session exposing (Session)
import Skeleton


type alias Model =
    { session : Session }


type Msg
    = Nop


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session }, Cmd.none )


update message model =
    case message of
        Nop ->
            ( model, Cmd.none )


view : Model -> Skeleton.Details Msg
view model =
    { title = "Notes"
    , header = []
    , warning = []
    , attrs = []
    , kids =
        [ div [ class "center pv3 ph3 ph0-l justify-between measure-wide" ] [ text "Notes" ] ]
    }
