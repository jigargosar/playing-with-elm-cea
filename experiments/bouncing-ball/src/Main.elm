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
-- * Make it pretty


svgView =
    let
        w =
            500

        h =
            round (w * 2 / 3)
    in
    Svg.svg [ HA.width w, HA.height h ]
        [ Svg.rect [ SA.width "100%", SA.height "100%", SA.fill "green" ] []
        , Svg.circle [ SA.cx "100", SA.cy "100", SA.r "50" ] []
        ]


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
        , svgView
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
