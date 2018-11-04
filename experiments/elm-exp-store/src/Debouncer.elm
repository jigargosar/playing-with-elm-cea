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
        scheduleEmit bouncedItem { count } =
            Task.perform (\_ -> EmitIfCountEq count bouncedItem |> config.toMsg)
                (Process.sleep config.wait)

        incCount =
            { model | count = model.count + 1 }
    in
    (case message of
        NoOp ->
            identity

        EmitIfCountEq count bouncedItem ->
            if model.count == count then
                replaceModel init
                    >> addMsg (config.onEmit bouncedItem)

            else
                identity

        Bounce bouncedItem ->
            replaceModel incCount
                >> (addEffect <| scheduleEmit bouncedItem)
    )
    <|
        pure model
