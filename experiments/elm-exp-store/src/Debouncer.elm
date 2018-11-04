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
    | EmitIfCountEq bouncedItem Int
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
update config message model =
    let
        incCount =
            { model | count = model.count + 1 }
    in
    (case message of
        NoOp ->
            identity

        EmitIfCountEq bouncedItem count ->
            if model.count == count then
                replaceModel init
                    >> addMsg (config.onEmit bouncedItem)

            else
                identity

        Bounce bouncedItem ->
            replaceModel incCount
                >> addEffect
                    (.count
                        >> EmitIfCountEq bouncedItem
                        >> afterTimeout config.wait
                        >> Cmd.map config.toMsg
                    )
    )
    <|
        pure model
