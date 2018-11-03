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
    | Push item


type alias Config msg item =
    { toMsg : Msg item -> msg
    , delay : Int
    }


push =
    Push


update : Config msg item -> Msg item -> Debouncer item -> ( Debouncer item, Cmd msg )
update config message model =
    let
        andThenUpdate =
            andThen << update config
    in
    (case message of
        NoOp ->
            identity

        Push item ->
            identity
    )
    <|
        pure model
