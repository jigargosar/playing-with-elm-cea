module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Color as ColorConverter
import Hsla
import Html as H exposing (Html)
import Html.Attributes as HA exposing (src)
import Svg
import Svg.Attributes as SA



---- MODEL ----


type alias Model =
    { bc : Hsla.HSLA }


init : ( Model, Cmd Msg )
init =
    ( { bc = Hsla.create 0.1 1 1 1 }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | BC Hsla.HSLA


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
    let
        c =
            ColorConverter.green |> ColorConverter.toHsla

        s =
            "hsla(116 , 60%, 80% ,1)"
    in
    c


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
        [ Svg.rect [ SA.width "100%", SA.height "100%", SA.fill (bc |> Hsla.toHexAString) ] []
        , Svg.circle
            [ SA.cx "100", SA.cy "100", SA.r (ballRadius |> String.fromInt), SA.fill blue ]
            []
        ]


viewHslaInput hslaC =
    let
        inputNum v =
            H.input
                [ HA.class "pa1 w3"
                , HA.type_ "number"
                , HA.value v
                ]
                []

        inputInt i =
            inputNum (String.fromInt i)
    in
    [ Hsla.hueInt hslaC |> inputInt
    , Hsla.saturationInt hslaC |> inputInt
    , Hsla.lightnessInt hslaC |> inputInt
    , hslaC.alpha |> String.fromFloat |> inputNum
    ]


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "pa3 vs3" ]
            [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
            , H.div [ HA.class "" ]
                (viewHslaInput model.bc)
            , H.div [ HA.class "" ] [ svgView model ]
            ]
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
