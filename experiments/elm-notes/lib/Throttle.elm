module Throttle exposing (..)

import Process
import Task


type alias Model =
    { ms : Float, scheduled : Bool }


init ms =
    Model ms False


push msg model =
    let
        ( newScheduled, cmd ) =
            if model.scheduled then
                ( model.scheduled, Cmd.none )
            else
                ( True
                , Process.sleep 3000
                    |> Task.perform (always msg)
                )
    in
        ( { model | scheduled = newScheduled }, cmd )


updateOnEmit model =
    { model | scheduled = False }
