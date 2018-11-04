module Debouncer exposing (Config, Debouncer, Msg, bounce, init, update)

import Process
import UpdateReturn exposing (..)


type alias Debouncer =
    { count : Int
    }


init =
    Debouncer 0


type Msg bouncedItem
    = NoOp
    | SetCount Int
    | IncCount
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


replaceModel m ( _, c ) =
    ( m, c )


update : Config msg bouncedItem -> Msg bouncedItem -> Debouncer -> ( Debouncer, Cmd msg )
update config message model =
    let
        andThenUpdate =
            andThen << update config
    in
    (case message of
        NoOp ->
            identity

        SetCount count ->
            replaceModel { model | count = count }

        IncCount ->
            andThenUpdate (SetCount <| model.count + 1)

        ScheduleEmit bouncedItem ->
            perform (\_ -> EmitIfCountEq model.count bouncedItem |> config.toMsg)
                (Process.sleep config.wait)

        EmitIfCountEq count bouncedItem ->
            if model.count == count then
                addMsg (config.onEmit bouncedItem)
                    >> replaceModel init

            else
                identity

        Bounce bouncedItem ->
            andThenUpdate IncCount
                >> andThenUpdate (ScheduleEmit bouncedItem)
    )
    <|
        pure model
