module Main exposing (main)

import Browser
import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as D
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
import Vec exposing (Vec)
import ViewSvg



---- MODEL ----


type alias Flags =
    { now : Int }


type alias Ball =
    Particle


type alias Ship =
    Particle


initialShip =
    Particle.new 0 0 1 0 50 0


type alias Model =
    { paused : Bool
    , balls : List Ball
    , seed : Random.Seed
    , ship : Ship
    , thrust : Vec
    , shipAngle : Float
    }


ballGenerator =
    let
        angleGenerator =
            let
                spread =
                    360
            in
            Random.float (90 - spread) (90 + spread)

        magnitudeGenerator =
            Random.float 2 7

        radiusGenerator =
            Random.float 4 4

        newBall mag ang r =
            Particle.new 0 100 mag ang r -0.1
    in
    Random.map3 newBall magnitudeGenerator angleGenerator radiusGenerator


initialModel fromSeed =
    let
        ( balls, seed ) =
            Random.step (Random.list 500 ballGenerator) fromSeed
    in
    { paused = False
    , balls = balls
    , seed = seed
    , ship = initialShip
    , thrust = Vec.zero
    , shipAngle = 0
    }


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
    | Step
    | Reset
    | Restart
    | Pause Bool
    | KeyDown String


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
            ter m.paused (pure m) (update Step m)

        Step ->
            pure
                { m
                    | balls = m.balls |> List.map Particle.update
                    , ship = m.ship |> Particle.update
                }

        Pause newPaused ->
            pure { m | paused = newPaused }

        KeyDown key ->
            let
                _ =
                    Debug.log "kD" key

                newModel =
                    case key of
                        "ArrowLeft" ->
                            { m | shipAngle = m.shipAngle - 0.1 }

                        "ArrowRight" ->
                            { m | shipAngle = m.shipAngle + 0.1 }

                        _ ->
                            m
            in
            pure newModel


pure m =
    ( m, Cmd.none )



---- VIEW ----


viewSvg m =
    Svg.svg [ SA.class "flex center", HA.width worldWidth, HA.height worldHeight ]
        (ViewSvg.view
            { balls = m.balls
            , ship = m.ship
            , shipAngle = m.shipAngle
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
        , hBtn [ HA.disabled (not paused) ] Step "Step"
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
        , Browser.Events.onKeyDown (D.field "key" D.string |> D.map KeyDown)
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
