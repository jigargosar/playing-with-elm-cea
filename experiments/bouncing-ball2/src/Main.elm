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


type alias Ball =
    Particle


type alias Model =
    { paused : Bool, balls : List Ball, seed : Random.Seed }


ballGenerator =
    let
        angleGenerator =
            Random.float 0 360

        magnitudeGenerator =
            Random.float 2 7

        radiusGenerator =
            Random.float 4 4

        newBall mag ang r =
            Particle.new 0 0 mag ang r -0.1
    in
    Random.map3 newBall magnitudeGenerator angleGenerator radiusGenerator


initialModel fromSeed =
    let
        ( balls, seed ) =
            Random.step (Random.list 500 ballGenerator) fromSeed
    in
    { paused = False, balls = balls, seed = seed }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    ( initialModel (Random.initialSeed now), Cmd.none )


worldWidth =
    600


worldHeight =
    {- round (worldWidth * 2 / 3) -}
    worldWidth


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
                        { m | balls = m.balls |> List.map Particle.update }
            in
            ter m.paused (pure m) ret

        Pause newPaused ->
            pure { m | paused = newPaused }


pure m =
    ( m, Cmd.none )



---- VIEW ----


viewSvg { balls } =
    Svg.svg [ SA.class "flex center", HA.width worldWidth, HA.height worldHeight ]
        (ViewSvg.view
            { balls = balls
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
            , H.div [ HA.class "", HE.onClick Restart ] [ viewSvg model ]
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
