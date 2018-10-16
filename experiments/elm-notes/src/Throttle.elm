module Throttle exposing (..)

import Process
import Task


type alias Model =
    { ms : Int, scheduled : Bool }


init ms =
    Model ms False


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
