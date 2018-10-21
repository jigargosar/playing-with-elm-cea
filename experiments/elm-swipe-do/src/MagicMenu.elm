module MagicMenu exposing (MagicMenu, initial)

import FeatherIcons
import Json.Decode as D
import Json.Encode as E
import Port
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Port.wheel Wheel ]


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        Clicked ->
            ( { model | open = not model.open }, Cmd.none )

        Wheel encoded ->
            D.decodeValue WheelEvent.decoder encoded
                |> mapDecodeResult model (\{ deltaY } -> { model | hidden = deltaY > 0 })


type alias Action msg =
    { icon : FeatherIcons.Icon, msg : msg }


type alias Actions msg =
    List (Action msg)


mapDecodeResult : model -> (answer -> model) -> Result D.Error answer -> ( model, Cmd msg )
mapDecodeResult model mapper result =
    case result of
        Ok answer ->
            ( mapper answer, Cmd.none )

        Err err ->
            ( model, D.errorToString err |> Port.logS )
