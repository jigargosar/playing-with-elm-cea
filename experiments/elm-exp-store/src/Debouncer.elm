module Debouncer exposing (Config, Debouncer, Msg, bounce, init, update)

import Process
import Task
import UpdateReturn exposing (..)


type alias Debouncer =
    { count : Int
    }


init =
    Debouncer 0


type Msg bouncedItem
    = NoOp
    | ScheduleEmit bouncedItem
    | EmitIfCountEq Int bouncedItem
    | Bounce bouncedItem


type alias Config msg bouncedItem =
    { toMsg : Msg bouncedItem -> msg
    , wait : Float
    , onEmit : bouncedItem -> msg
    }


bounce : bouncedItem -> Msg bouncedItem
bounce =
    Bounce


addEffect fn ( m, c ) =
    ( m, Cmd.batch [ c, fn m ] )


update : Config msg bouncedItem -> Msg bouncedItem -> Debouncer -> ( Debouncer, Cmd msg )
update config message model =
    let
        andThenUpdate =
            andThen << update config

        addEmitEffect bouncedItem =
            addEffect
                (\{ count } ->
                    Task.perform (\_ -> EmitIfCountEq count bouncedItem |> config.toMsg)
                        (Process.sleep config.wait)
                )

        setCount count =
            { model | count = count }

        incCount =
            setCount <| model.count + 1
    in
    (case message of
        NoOp ->
            identity

        ScheduleEmit bouncedItem ->
            perform (\_ -> EmitIfCountEq model.count bouncedItem |> config.toMsg)
                (Process.sleep config.wait)

        EmitIfCountEq count bouncedItem ->
            if model.count == count then
                replaceModel init
                    >> addMsg (config.onEmit bouncedItem)

            else
                identity

        Bounce bouncedItem ->
            replaceModel incCount
                >> addEmitEffect bouncedItem
    )
    <|
        pure model
