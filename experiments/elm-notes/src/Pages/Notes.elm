module Pages.Notes exposing (..)

import Session exposing (Session)


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
