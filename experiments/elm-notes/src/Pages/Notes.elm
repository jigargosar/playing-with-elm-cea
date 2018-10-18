module Pages.Notes exposing (..)

import Html exposing (..)
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
    { title = "Notes List"
    , header = []
    , warning = []
    , attrs = []
    , kids =
        [ div [] [ text "Home View" ] ]
    }
