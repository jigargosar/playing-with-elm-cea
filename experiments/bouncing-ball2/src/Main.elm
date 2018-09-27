module Main exposing (main)

import Browser
import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Round
import Set
import String exposing (String)
import Svg
import Svg.Attributes as SA



---- MODEL ----


type alias Flags =
    {}


type alias Model =
    { x : Int }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { x = 100 }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | AFrame Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        AFrame delta ->
            ( m
            , Cmd.none
            )



---- VIEW ----


svgView { x } =
    let
        w =
            500

        h =
            round (w * 2 / 3)

        ballRadius =
            10
    in
    Svg.svg [ HA.width w, HA.height h ]
        [ Svg.rect [ SA.width "100%", SA.height "100%", SA.fill "#adbeeb" ] []
        , Svg.circle
            [ x |> String.fromInt >> SA.cx
            , SA.cy "100"
            , SA.r (ballRadius |> String.fromInt)
            , SA.fill "#cd37a9"
            ]
            []
        ]


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "pa3 vs3" ]
            [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
            , H.div [ HA.class "" ] [ svgView model ]
            ]
        ]



---- Subscriptions ----


subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AFrame
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
