module Debouncer exposing (Config, Debouncer, Msg, bounce, init, update)

import Process
import UpdateReturn exposing (..)


type alias Debouncer item =
    { latest : Maybe item
    , count : Int
    }


init =
    Debouncer Nothing 0


type Msg bouncedItem
    = NoOp
    | SetLatest (Maybe bouncedItem)
    | SetCount Int
    | IncCount
    | ScheduleEmit
    | EmitIfCountEq Int
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


setCount : Int -> Debouncer bouncedItem -> Debouncer bouncedItem
setCount count model =
    { model | count = count }


update : Config msg bouncedItem -> Msg bouncedItem -> Debouncer bouncedItem -> ( Debouncer bouncedItem, Cmd msg )
update config message model =
    let
        andThenUpdate =
            andThen << update config
    in
    (case message of
        NoOp ->
            identity

        SetLatest latest ->
            replaceModel { model | latest = latest }

        SetCount count ->
            replaceModel { model | count = count }

        IncCount ->
            andThenUpdate (SetCount <| model.count + 1)

        ScheduleEmit ->
            perform (\_ -> EmitIfCountEq model.count |> config.toMsg)
                (Process.sleep config.wait)

        EmitIfCountEq count ->
            case ( model.count == count, model.latest ) of
                ( True, Just bouncedItem ) ->
                    addMsg (config.onEmit bouncedItem)
                        >> replaceModel init

                _ ->
                    identity

        Bounce bouncedItem ->
            andThenUpdate (SetLatest <| Just bouncedItem)
                >> andThenUpdate IncCount
                >> andThenUpdate ScheduleEmit
    )
    <|
        pure model
