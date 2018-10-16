module Throttle exposing (..)

import Process
import Task


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
            let
                _ =
                    1
            in
                if model.scheduled then
                    ( model, Cmd.none )
                else
                    ( { model | scheduled = True }
                    , Process.sleep model.ms
                        |> Task.perform (always Trigger)
                    )
