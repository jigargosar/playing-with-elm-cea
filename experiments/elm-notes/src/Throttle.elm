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


type Msg
    = Emit
    | EventOccurred


type Reply
    = Emitted


update msg model =
    let
        _ =
            Debug.log "Trigger: Pre Update" ( msg, model )
    in
        (case msg of
            Emit ->
                ( { model | scheduled = False }, Cmd.none, Just Emitted )

            EventOccurred ->
                if model.scheduled then
                    ( model, Cmd.none, Nothing )
                else
                    ( { model | scheduled = True }
                    , Process.sleep model.ms
                        |> Task.perform (always Emit)
                    , Nothing
                    )
        )
            |> Debug.log "Trigger: Post Update"
