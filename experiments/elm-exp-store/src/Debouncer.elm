module Debouncer exposing (Config, Debouncer, Msg, init, push, update)

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
    | Push item
    | IncCount


type alias Config msg item =
    { toMsg : Msg item -> msg
    , delay : Int
    }


push =
    Push


setLatest : Maybe item -> Debouncer item -> Debouncer item
setLatest latest model =
    { model | latest = latest }


update : Config msg item -> Msg item -> Debouncer item -> ( Debouncer item, Cmd msg )
update config message model =
    let
        andThenUpdate =
            andThen << update config
    in
    (case message of
        NoOp ->
            identity

        SetLatest item ->
            Tuple.mapFirst (setLatest item)

        Push item ->
            andThenUpdate (SetLatest <| Just item)
    )
    <|
        pure model
