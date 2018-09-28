module Main exposing (main)

import Browser
import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Particle exposing (Particle)
import Ramda exposing (ter)
import Random
import Round
import Set
import String exposing (String)
import Svg
import Svg.Attributes as SA
import Time
import TypedSvg as TS
import TypedSvg.Attributes as TA
import TypedSvg.Types as TT
import Vec
import ViewSvg



---- MODEL ----


type alias Flags =
    { now : Int }


type alias Model =
    { paused : Bool, ball : Particle, seed : Random.Seed }


initialModel fromSeed =
    let
        angleGenerator =
            Random.float 0 360

        magnitudeGenerator =
            Random.float 0.5 2.5

        velocityGenerator =
            Random.map2 Vec.newMA magnitudeGenerator angleGenerator

        radiusGenerator =
            Random.float 5 20

        ballGenerator =
            let
                newBall vel r =
                    Particle.fromRec { pos = Vec.zero, vel = vel, r = r }
            in
            Random.map2 newBall velocityGenerator radiusGenerator

        ( ball, seed ) =
            Random.step ballGenerator fromSeed
    in
    { paused = False, ball = ball, seed = seed }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    ( initialModel (Random.initialSeed now), Cmd.none )


worldWidth =
    500


worldHeight =
    round (worldWidth * 2 / 3)


worldSizeVec =
    Vec.fromInt worldWidth worldHeight



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
            update (Pause True) (initialModel m.seed)

        Restart ->
            update (Pause False) (initialModel m.seed)

        AFrame delta ->
            let
                vel =
                    Vec.newXY 1.5 0

                ret =
                    pure
                        { m | ball = Particle.update m.ball }
            in
            ter m.paused (pure m) ret

        Pause newPaused ->
            pure { m | paused = newPaused }


pure m =
    ( m, Cmd.none )



---- VIEW ----


viewSvg { ball } =
    Svg.svg [ HA.width worldWidth, HA.height worldHeight ]
        (ViewSvg.svgView
            { ball = ball
            , worldSize = worldSizeVec
            }
        )


hBtn al msg lt =
    H.button ([ HE.onClick msg ] ++ al) [ H.text lt ]


viewControls { paused } =
    H.div [ HA.class "hs2" ]
        [ hBtn [] Reset "Reset"
        , hBtn [] Restart "Restart"
        , hBtn [] (Pause (not paused)) (ter paused "Play" "Pause")
        ]


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
