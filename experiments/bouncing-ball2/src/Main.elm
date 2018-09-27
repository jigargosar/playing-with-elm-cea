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


type alias Vec =
    ( Float, Float )


vec : Float -> Float -> Vec
vec x y =
    ( x, y )


vecAdd ( x, y ) ( x_, y_ ) =
    vec (x + x_) (y + y_)


type alias Model =
    { x : Float, pos : Vec }


initialModel =
    { x = 100, pos = vec 100 100 }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


worldWidth =
    500


worldHeight =
    round (worldWidth * 2 / 3)


ballRadius =
    30



---- UPDATE ----


type Msg
    = NoOp
    | AFrame Float
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        Reset ->
            ( initialModel, Cmd.none )

        AFrame delta ->
            let
                vel =
                    vec 1.5 0

                newPos =
                    m.pos |> vecAdd vel
            in
            ( { m | x = m.x + 1.5, pos = newPos }, Cmd.none )



--        AFrame delta ->
--            let
--                xPxPF =
--                    1.5
--
--                addPPF x =
--                    x + xPxPF
--
--                loopX2 x =
--                    if x - ballRadius > worldWidth then
--                        -ballRadius
--
--                    else
--                        x
--            in
--            ( { m | x = m.x |> addPPF >> loopX2 }, Cmd.none )
---- VIEW ----


svgView { x } =
    Svg.svg [ HA.width worldWidth, HA.height worldHeight ]
        [ Svg.rect [ SA.width "100%", SA.height "100%", SA.fill "#adbeeb" ] []
        , Svg.circle
            [ x |> String.fromFloat >> SA.cx
            , SA.cy "100"
            , ballRadius |> String.fromFloat >> SA.r
            , SA.fill "#cd37a9"
            ]
            []
        ]


viewControls _ =
    H.div [] [ H.button [ HE.onClick Reset, HA.autofocus True ] [ H.text "Reset" ] ]


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "pa3 vs3" ]
            [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
            , viewControls model
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
