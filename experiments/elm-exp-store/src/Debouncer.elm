module Debouncer exposing (Config, Debouncer, Msg, bounce, init, update)

import BasicsX exposing (..)
import Process
import Task
import UpdateReturn exposing (..)


type alias Debouncer =
    { count : Int
    }


init =
    Debouncer 0


incCount model =
    { model | count = model.count + 1 }


type Msg bouncedItem
    = EmitIfCountEq bouncedItem Int
    | Bounce bouncedItem


type alias Config msg bouncedItem =
    { toMsg : Msg bouncedItem -> msg
    , wait : Float
    , onEmit : bouncedItem -> msg
    }


bounce : bouncedItem -> Msg bouncedItem
bounce =
    Bounce


update : Config msg bouncedItem -> Msg bouncedItem -> Debouncer -> ( Debouncer, Cmd msg )
update config message =
    (case message of
        EmitIfCountEq bouncedItem count ->
            andThenIf (.count >> eqs count)
                (\_ -> pure init |> addMsg (config.onEmit bouncedItem))

        Bounce bouncedItem ->
            mapModel incCount
                >> addEffect
                    (.count
                        >> EmitIfCountEq bouncedItem
                        >> afterTimeout config.wait
                        >> Cmd.map config.toMsg
                    )
    )
        << pure
