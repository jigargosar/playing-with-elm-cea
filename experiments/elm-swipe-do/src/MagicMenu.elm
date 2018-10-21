module MagicMenu exposing (MagicMenu, initial)

import Json.Decode as D
import Json.Encode as E
import WheelEvent


type alias MagicMenu =
    { open : Bool
    , hidden : Bool
    }


type alias Model =
    MagicMenu


initial =
    MagicMenu False False


type Msg
    = NoOp
    | Clicked
    | Wheel E.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Clicked ->
            ( { model | open = not model.open }, Cmd.none )

        Wheel encoded ->
            ( D.decodeValue WheelEvent.decoder encoded
                |> Result.map (\{ deltaY } -> { model | hidden = deltaY > 0 })
                |> Result.withDefault model
            , Cmd.none
            )
