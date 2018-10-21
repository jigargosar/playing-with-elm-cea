module MagicMenu exposing (MagicMenu, initial)

import Json.Decode as D
import Json.Encode as E


type alias MagicMenu =
    { open : Bool
    , hidden : Bool
    }


initial =
    MagicMenu False False


type Msg
    = NoOp
    | Clicked
    | Wheel E.Value


update : Msg -> MagicMenu -> ( MagicMenu, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Clicked ->
            ( { model | open = not model.open }, Cmd.none )

        Wheel encoded ->
            ( model, Cmd.none )
