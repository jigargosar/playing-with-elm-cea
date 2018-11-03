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
    case message of
        NoOp ->
            ( model, Cmd.none )

        Push item ->
            pure model
