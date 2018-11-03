module Debouncer exposing (Config, Debouncer, Msg, init, update)


type alias Debouncer a =
    { latest : Maybe a
    , count : Int
    }


init =
    Debouncer Nothing 0


type Msg
    = NoOp


type alias Config msg =
    { toMsg : Msg -> msg
    , delay : Int
    }


update : Config msg -> Msg -> Debouncer a -> ( Debouncer a, Cmd msg )
update config message model =
    case message of
        NoOp ->
            ( model, Cmd.none )
