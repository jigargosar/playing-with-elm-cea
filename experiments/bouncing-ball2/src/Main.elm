module Main exposing (main)

import Browser
import Browser.Events
import El exposing (btn, globalStyle)
import Element exposing (layout, row, spacing)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Particle exposing (Particle)
import Round
import Set
import String exposing (String)
import Svg
import Svg.Attributes as SA
import Time
import TypedSvg as TS
import TypedSvg.Attributes as TA
import TypedSvg.Types as TT
import Vec exposing (Vec)
import ViewSvg



---- MODEL ----


type alias Flags =
    {}


type alias Model =
    { pos : Vec.Vec, paused : Bool, ball : Particle }


initialModel =
    { pos = Vec.new 0 0, paused = False, ball = Particle.zero |> Particle.setVelXY 1.5 0 }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


worldWidth =
    500


worldHeight =
    round (worldWidth * 2 / 3)


worldSizeVec =
    Vec.fromInt worldWidth worldHeight


ballRadius =
    30



---- UPDATE ----


type Msg
    = NoOp
    | AFrame Float
    | Reset
    | Restart
    | Pause Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        Reset ->
            update (Pause True) initialModel

        Restart ->
            update (Pause False) initialModel

        AFrame delta ->
            let
                vel =
                    Vec.new 1.5 0

                ret =
                    pure
                        { m
                            | pos = Vec.add m.pos vel
                            , ball = Particle.update m.ball
                        }
            in
            ter m.paused (pure m) ret

        Pause newPaused ->
            pure { m | paused = newPaused }


pure m =
    ( m, Cmd.none )



---- VIEW ----


viewSvg { pos } =
    Svg.svg [ HA.width worldWidth, HA.height worldHeight ]
        (ViewSvg.svgView { ballPos = pos, ballRadius = ballRadius, worldSize = worldSizeVec })


ter bool v1 v2 =
    if bool then
        v1

    else
        v2


viewControls { paused } =
    layout globalStyle
        (row [ spacing 8 ]
            [ btn [] Reset "Reset"
            , btn [] Restart "Restart"
            , btn [] (Pause (not paused)) (ter paused "Play" "Pause")
            ]
        )


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "pa3 vs3" ]
            [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
            , viewControls model
            , H.div [ HA.class "" ] [ viewSvg model ]
            ]
        ]



---- Subscriptions ----


subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AFrame

        {- , Time.every (5 * 1000) (\_ -> Reset) -}
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
