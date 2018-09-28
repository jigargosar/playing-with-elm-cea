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
import Time



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


vecXStr =
    Tuple.first >> String.fromFloat


type alias Model =
    { pos : Vec, paused : Bool }


initialModel =
    { pos = vec 100 100, paused = False }


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
    | TogglePause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        Reset ->
            pure initialModel

        AFrame delta ->
            let
                vel =
                    vec 1.5 0

                ret =
                    pure { m | pos = vecAdd m.pos vel }
            in
            ter m.paused (pure m) ret

        TogglePause ->
            pure { m | paused = not m.paused }


pure m =
    ( m, Cmd.none )



---- VIEW ----


svgView { pos } =
    Svg.svg [ HA.width worldWidth, HA.height worldHeight ]
        [ Svg.g []
            [ Svg.rect [ SA.width "100%", SA.height "100%", SA.fill "#adbeeb" ] []
            , Svg.g
                [ SA.transform
                    ([ "translate("
                     , worldWidth / 2 |> String.fromFloat
                     , ","
                     , toFloat worldHeight / 2 |> String.fromFloat
                     , "0)"
                     ]
                        |> String.join ""
                    )
                ]
                [ Svg.circle
                    [ pos |> vecXStr >> SA.cx
                    , SA.cy "100"
                    , ballRadius |> String.fromFloat >> SA.r
                    , SA.fill "#cd37a9"
                    ]
                    []
                ]
            ]
        ]


ter bool v1 v2 =
    if bool then
        v1

    else
        v2


viewControls { paused } =
    H.div []
        [ H.button [ HE.onClick Reset, HA.autofocus True ] [ H.text "Reset" ]
        , H.button [ HE.onClick TogglePause ] [ H.text (ter paused "Play" "Pause") ]
        ]


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
        , Time.every (5 * 1000) (\_ -> Reset)
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
