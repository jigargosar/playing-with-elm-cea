module Throttle exposing (..)


type alias Model =
    { ms : Int, scheduled : Bool }


type Msg
    = Trigger
    | Event


update msg model =
    case msg of
        Trigger ->
            ( model, Cmd.none )

        Event ->
            ( model, Cmd.none )
