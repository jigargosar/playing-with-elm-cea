module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA exposing (src)
import Svg
import Svg.Attributes as SA



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----
---- Step 1 draw svg ----


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
        , Svg.svg [] [ Svg.circle [ SA.cx "100", SA.cy "100", SA.r "50" ] [] ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
