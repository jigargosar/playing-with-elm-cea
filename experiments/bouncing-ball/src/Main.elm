module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA exposing (src)
import Svg
import Svg.Attributes as SA



---- MODEL ----


type alias Model =
    { bc : String }


init : ( Model, Cmd Msg )
init =
    ( { bc = green }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | BC String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        BC bc ->
            ( { m | bc = bc }, Cmd.none )



---- VIEW ----
---- Step 1 draw svg ----
-- * Make it pretty


green =
    "hsla(116 , 60%, 80% ,1)"


blue =
    "hsla(250, 70%, 50% ,1)"


svgView { bc } =
    let
        w =
            500

        h =
            round (w * 2 / 3)

        ballRadius =
            10
    in
    Svg.svg [ HA.width w, HA.height h ]
        [ Svg.rect [ SA.width "100%", SA.height "100%", SA.fill bc ] []
        , Svg.circle
            [ SA.cx "100", SA.cy "100", SA.r (ballRadius |> String.fromInt), SA.fill blue ]
            []
        ]


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
        , H.div [] [ H.input [ HA.type_ "number", HA.value "foo" ] [] ]
        , svgView model
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
