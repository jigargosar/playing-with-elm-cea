module Throttle exposing (..)

import Process
import Return2
import Task


type alias Model =
    { ms : Int, scheduled : Bool }


type Msg
    = Trigger
    | Event


update msg model =
    let
        _ =
            Debug.log "Trigger: Pre Update" ( msg, model )
    in
        (case msg of
            Trigger ->
                let
                    _ =
                        Debug.log "OnTrigger" model
                in
                    ( { model | scheduled = False }, Cmd.none )

            Event ->
                let
                    _ =
                        Debug.log "OnEventQueued" model
                in
                    if model.scheduled then
                        ( model, Cmd.none )
                    else
                        ( { model | scheduled = True }
                        , Process.sleep model.ms
                            |> Task.perform (always Trigger)
                        )
        )
            |> Debug.log "Trigger: Post Update"
