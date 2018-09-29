module Main exposing (main)

import Array
import BasicsX exposing (vec2FromPair, vec2ToPair)
import Browser
import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy
import Json.Decode as D
import List.Extra
import Math.Vector2 as V exposing (Vec2)
import Particle exposing (Particle)
import Ramda exposing (subBy, ter)
import Random
import Round
import Set exposing (Set)
import String exposing (String)
import Svg
import Svg.Attributes as SA
import Time
import Tuple2
import TypedSvg as TS
import TypedSvg.Attributes as TA
import TypedSvg.Types as TT
import ViewSvg



---- MODEL ----


type alias Flags =
    { now : Int }


type alias Ball =
    Particle


type alias Ship =
    Particle


dp =
    { x = 0, y = 0, vm = 0, va = 0, r = 10, am = 0, aa = 0, mass = 1 }


type alias Model =
    { paused : Bool
    , balls : List Ball
    , seed : Random.Seed
    , ship : Ship
    , shipAngle : Float
    , shipThrust : Float
    , keyDownSet : Set String
    , sun : Particle
    , planet : Particle
    , warpBall : Particle
    }


ballGenerator =
    let
        angleGenerator =
            let
                spread =
                    360
            in
            Random.float (-90 - spread) (-90 + spread)

        magnitudeGenerator =
            Random.float 2 7

        radiusGenerator =
            Random.float 4 4

        newBall vm va r =
            Particle.new
                { dp
                    | x = 0
                    , y = -100
                    , vm = vm
                    , va = va
                    , r = r
                    , am = 0.1
                    , aa = 90
                }
    in
    Random.map3 newBall magnitudeGenerator angleGenerator radiusGenerator


initialModel fromSeed =
    let
        ( balls, seed ) =
            Random.step (Random.list 500 ballGenerator) fromSeed

        sun =
            Particle.new { dp | r = 20, mass = 1000 }

        initialShip =
            Particle.new { dp | x = 200, vm = 2, va = 90, r = 50 }
    in
    { paused = False
    , balls = balls
    , seed = seed
    , ship = initialShip
    , shipAngle = 0
    , shipThrust = 0
    , keyDownSet = Set.empty
    , sun = sun
    , planet = Particle.new { dp | y = 200, vm = 2, va = 0, r = 5 }
    , warpBall = Particle.new { dp | vm = 10, va = -180, r = 50 }
    }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    ( initialModel (Random.initialSeed now), Cmd.none )


worldWidth =
    600


worldHeight =
    {- round (worldWidth * 2 / 3) -}
    worldWidth


worldSize =
    V.vec2 (toFloat worldWidth) (toFloat worldHeight)


isKeyDown key m =
    Set.member key m.keyDownSet



---- UPDATE ----


type Msg
    = NoOp
    | AFrame Float
    | Step
    | Reset
    | Restart
    | SetPause Bool
    | Pause
    | Play
    | TogglePause
    | KeyDown String
    | KeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        Reset ->
            update Pause (initialModel m.seed)

        Restart ->
            update Play (initialModel m.seed)

        AFrame delta ->
            ter m.paused (pure m) (update Step m)

        Step ->
            m
                |> updateParticles
                |> pure

        SetPause newPaused ->
            pure { m | paused = newPaused }

        TogglePause ->
            update (SetPause (not m.paused)) m

        Pause ->
            update (SetPause True) m

        Play ->
            update (SetPause False) m

        KeyDown key ->
            pure { m | keyDownSet = Set.insert key m.keyDownSet }

        KeyUp key ->
            pure { m | keyDownSet = Set.remove key m.keyDownSet }


computeNewShipAngle m =
    (if isKeyDown "ArrowLeft" m then
        -1

     else if isKeyDown "ArrowRight" m then
        1

     else
        0
    )
        |> (*) 5
        |> (+) m.shipAngle


computeNewShipThrust m =
    case isThrusting m of
        True ->
            vec2FromPair (fromPolar ( 0.1, degrees (computeNewShipAngle m) ))

        False ->
            V.vec2 0 0


updateParticles m =
    { m
        | balls = m.balls |> List.map (Particle.acc (V.vec2 0 0.1) >> Particle.update)
        , ship =
            m.ship
                |> Particle.acc
                    (V.add (computeNewShipThrust m) (computeShipGravity m))
                >> Particle.update
        , shipAngle = computeNewShipAngle m
        , planet = m.planet |> Particle.acc (computePlanetGravity m) >> Particle.update
        , warpBall = m.warpBall |> Particle.update |> Particle.warp worldSize
    }


pure m =
    ( m, Cmd.none )


isThrusting m =
    isKeyDown "ArrowUp" m


computePlanetGravity m =
    let
        ( sunPos, planetPos ) =
            ( m.sun, m.planet ) |> Tuple2.mapBoth Particle.getPos

        length =
            Particle.getMass m.sun / V.distanceSquared sunPos planetPos

        angle =
            V.sub sunPos planetPos |> vec2ToPair >> toPolar >> Tuple.second
    in
    ( length, angle ) |> fromPolar >> vec2FromPair


computeShipGravity m =
    let
        ( sunPos, planetPos ) =
            ( m.sun, m.ship ) |> Tuple2.mapBoth Particle.getPos

        length =
            Particle.getMass m.sun / V.distanceSquared sunPos planetPos

        angle =
            V.sub sunPos planetPos |> vec2ToPair >> toPolar >> Tuple.second
    in
    ( length, angle ) |> fromPolar >> vec2FromPair



---- VIEW ----


hBtn al msg lt =
    H.button ([ HE.onClick msg ] ++ al) [ H.text lt ]


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "pa3 vs3" ]
            [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
            , viewControls model
            , Html.Lazy.lazy viewContent model
            ]
        ]


viewControls { paused } =
    H.div [ HA.class "hs2 no-sel" ]
        [ hBtn [] Reset "Reset"
        , hBtn [] Restart "Restart"
        , hBtn [] TogglePause (ter paused "Play" "Pause")
        , hBtn [ HA.disabled (not paused) ] Step "Step"
        ]


viewContent m =
    H.div [ HA.class "no-sel", HE.onDoubleClick Restart ]
        [ Svg.svg
            [ SA.class "flex center"
            , HA.width worldWidth
            , HA.height worldHeight
            , HE.onBlur Play
            ]
            (ViewSvg.view worldSize
                [ ViewSvg.viewParticle m.warpBall "pink"
                , ViewSvg.viewBalls m.balls
                , ViewSvg.viewParticle m.sun "orange"
                , ViewSvg.viewParticle m.planet "red"
                , ViewSvg.viewShip m.ship m.shipAngle (isThrusting m)
                ]
            )
        ]



---- Subscriptions ----


subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AFrame
        , Browser.Events.onKeyDown (D.field "key" D.string |> D.map KeyDown)
        , Browser.Events.onKeyUp (D.field "key" D.string |> D.map KeyUp)

        --        , Browser.Events.onKeyPress (D.field "key" D.string |> D.map KeyUp)
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
