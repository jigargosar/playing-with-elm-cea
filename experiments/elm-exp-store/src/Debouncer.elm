module Debouncer exposing (Config, Debouncer, Msg, init, push, update)

import Process
import UpdateReturn exposing (..)


type alias Debouncer item =
    { latest : Maybe item
    , count : Int
    }


init =
    Debouncer Nothing 0


type Msg item
    = NoOp
    | SetLatest (Maybe item)
    | SetCount Int
    | IncCount
    | ScheduleEmit
    | EmitIfCountEq Int
    | Push item


type alias Config msg item =
    { toMsg : Msg item -> msg
    , wait : Float
    , onEmit : item -> msg
    }


push =
    Push


replaceModel m ( _, c ) =
    ( m, c )


setCount : Int -> Debouncer item -> Debouncer item
setCount count model =
    { model | count = count }


update : Config msg item -> Msg item -> Debouncer item -> ( Debouncer item, Cmd msg )
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
                ( True, Just item ) ->
                    addMsg (config.onEmit item)
                        >> replaceModel init

                _ ->
                    identity

        Push item ->
            andThenUpdate (SetLatest <| Just item)
                >> andThenUpdate IncCount
                >> andThenUpdate ScheduleEmit
    )
    <|
        pure model
