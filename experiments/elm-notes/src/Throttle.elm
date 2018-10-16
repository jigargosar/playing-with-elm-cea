module Throttle exposing (..)

import Process
import Return2
import Task


type alias Model =
    { ms : Int, scheduled : Bool }


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
