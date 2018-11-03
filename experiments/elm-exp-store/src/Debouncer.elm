module Debouncer exposing (Config, Debouncer, Msg, init, push, update)

import UpdateReturn exposing (..)


type alias Debouncer a =
    { latest : Maybe a
    , count : Int
    }


init =
    Debouncer Nothing 0


type Msg a
    = NoOp
    | Push a


type alias Config msg =
    { toMsg : Msg -> msg
    , delay : Int
    }


push =
    Push


update : Config msg -> Msg a -> Debouncer a -> ( Debouncer a, Cmd msg )
update config message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Push item ->
            pure model
