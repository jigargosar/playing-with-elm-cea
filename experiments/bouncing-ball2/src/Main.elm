module Main exposing (main)

import Array
import BasicsX exposing (vec2FromPair, vec2ToPair)
import Browser
import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy
import Http
import Json.Decode as D
import List.Extra
import Math.Vector2 as V exposing (Vec2)
import Particle as P exposing (Particle)
import Ramda exposing (appendTo, flip, ifElse, subBy, ter)
import Random
import Round
import Set exposing (Set)
import String exposing (String)
import String.Conversions
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


part =
    { x = 0, y = 0, vm = 0, va = 0, r = 10, mass = 1 }


type alias Snapshot =
    { balls : List Ball
    , ship : Ship
    , shipAngle : Float
    , shipThrust : Float
    , sun : Particle
    , planet : Particle
    , warpBall : Particle
    }


type alias SimulationHistory =
    List Snapshot


type SimulationStatus
    = Running
    | Paused Int


type alias Simulation =
    { history : SimulationHistory, status : SimulationStatus }


type alias Model =
    { balls : List Ball
    , seed : Random.Seed
    , ship : Ship
    , shipAngle : Float
    , shipThrust : Float
    , keyDownSet : Set String
    , sun : Particle
    , planet : Particle
    , warpBall : Particle
    , stats : { ballCount : Int, ship : Ship }
    , connected : Bool
    , simulation : Simulation
    }


ballGenerator =
    let
        angleG =
            let
                spread =
                    360

                angle =
                    -90
            in
            Random.float (angle - spread) (angle + spread)

        magnitudeG =
            Random.float 2 7

        radiusG =
            Random.float 5 10

        newBall vm va r =
            P.new
                { part
                    | x = 0
                    , y = -100
                    , vm = vm
                    , va = va
                    , r = r
                }
    in
    Random.map3 newBall magnitudeG angleG radiusG


generateBalls ct seed =
    Random.step (Random.list ct ballGenerator) seed


initialModel : Random.Seed -> Model
initialModel fromSeed =
    let
        ( balls, seed ) =
            generateBalls 500 fromSeed

        sun =
            P.new { part | r = 20, mass = 1000 }

        ship =
            P.new { part | x = 200, vm = 2, va = 90, r = 50 }

        planet =
            P.new { part | y = 200, vm = 2, va = 0, r = 5 }
    in
    { balls = balls
    , seed = seed
    , ship = ship
    , shipAngle = 0
    , shipThrust = 0
    , keyDownSet = Set.empty
    , sun = sun
    , planet = planet
    , warpBall = P.new { part | x = 0, y = 0, vm = 5, va = 25, r = 50 }
    , stats = { ballCount = 0, ship = ship }
    , connected = True
    , simulation = { status = Running, history = [] }
    }
        |> updateStats
        |> updateHistory


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


isPaused =
    isRunning >> not


isRunning m =
    m.simulation.status == Running



---- UPDATE ----


type Msg
    = NoOp
    | AFrame Float
    | StepForward
    | StepBack
    | Step Int
    | Reset
    | Restart
    | SetPause Bool
    | Pause
    | Resume
    | TogglePause
    | KeyDown String
    | KeyUp String
    | Stats
    | EverySecond
    | Every5Second
    | Connection (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        Connection (Result.Err err) ->
            let
                _ =
                    Debug.log "err" err
            in
            pure { m | connected = False }

        Connection (Result.Ok _) ->
            pure { m | connected = True }

        Every5Second ->
            ( m, Http.send Connection <| Http.getString "/" )

        EverySecond ->
            update Stats m

        Stats ->
            ter (isRunning m) (updateStats m) m |> pure

        Reset ->
            update Pause (initialModel m.seed)

        Restart ->
            update Resume (initialModel m.seed)

        AFrame delta ->
            ifElse isRunning (update StepForward) pure m

        StepForward ->
            update (Step 1) m

        StepBack ->
            update (Step -1) m

        Step i ->
            let
                normalStep =
                    updateParticles
                        >> updateHistory
                        >> pure
            in
            case ( m.simulation.status, abs i == i ) of
                ( Paused 0, True ) ->
                    normalStep m

                ( Paused n, _ ) ->
                    let
                        { history } =
                            m.simulation

                        newIndex =
                            n
                                - i
                                |> clamp 0 (List.length history - 1)
                                |> Debug.log "newIndex"
                    in
                    { m
                        | simulation =
                            { status = Paused newIndex, history = history }
                    }
                        |> pure

                ( Running, _ ) ->
                    normalStep m

        SetPause newPaused ->
            (if newPaused then
                { m | simulation = { status = Paused 0, history = m.simulation.history } }

             else
                { m | simulation = { status = Running, history = m.simulation.history } }
            )
                |> updateStats
                |> pure

        TogglePause ->
            update (SetPause (isRunning m)) m

        Pause ->
            update (SetPause True) m

        Resume ->
            update (SetPause False) m

        KeyDown key ->
            pure { m | keyDownSet = Set.insert key m.keyDownSet }

        KeyUp key ->
            pure { m | keyDownSet = Set.remove key m.keyDownSet }


addCmd cmd =
    Tuple.mapSecond (appendTo [ cmd ] >> Cmd.batch)


updateStats m =
    { m | stats = { ballCount = m.balls |> List.length, ship = m.ship } }


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


isBeyondBottomEdge p =
    let
        ( x, y ) =
            P.getPosPair p
    in
    y > worldHeight / 2


updateHistory m =
    let
        sim =
            m.simulation
    in
    { m | simulation = { sim | history = getCurrentSnapshot m :: sim.history } }


getCurrentSnapshot : Model -> Snapshot
getCurrentSnapshot { balls, ship, shipAngle, shipThrust, sun, planet, warpBall } =
    { balls = balls
    , ship = ship
    , shipAngle = shipAngle
    , shipThrust = shipThrust
    , sun = sun
    , planet = planet
    , warpBall = warpBall
    }


updateParticles m =
    let
        oldBalls =
            m.balls |> List.filter (isBeyondBottomEdge >> not)

        ( newBalls, seed ) =
            {- generateBalls 1 m.seed -}
            ( [], m.seed )
    in
    { m
        | balls =
            oldBalls
                |> List.append newBalls
                |> List.map (P.acc (V.vec2 0 0.1) >> P.update)
        , seed = seed
        , ship =
            m.ship
                |> P.acc
                    (V.add (computeNewShipThrust m) (computeShipGravity m))
                >> P.update
        , shipAngle = computeNewShipAngle m
        , planet = m.planet |> P.acc (computePlanetGravity m) >> P.update
        , warpBall = m.warpBall |> P.update |> P.warp worldSize
    }
        |> updateHistory


pure m =
    ( m, Cmd.none )


isThrusting m =
    isKeyDown "ArrowUp" m


computePlanetGravity m =
    let
        ( sunPos, planetPos ) =
            ( m.sun, m.planet ) |> Tuple2.mapBoth P.getPos

        length =
            P.getMass m.sun / V.distanceSquared sunPos planetPos

        angle =
            V.sub sunPos planetPos |> vec2ToPair >> toPolar >> Tuple.second
    in
    ( length, angle ) |> fromPolar >> vec2FromPair


computeShipGravity m =
    let
        ( sunPos, planetPos ) =
            ( m.sun, m.ship ) |> Tuple2.mapBoth P.getPos

        length =
            P.getMass m.sun / V.distanceSquared sunPos planetPos

        angle =
            V.sub sunPos planetPos |> vec2ToPair >> toPolar >> Tuple.second
    in
    ( length, angle ) |> fromPolar >> vec2FromPair



---- VIEW ----


hBtn al msg lt =
    H.button ([ HE.onClick msg ] ++ al) [ H.text lt ]


view : Model -> Html Msg
view m =
    H.div []
        [ viewAlert m
        , H.div [ HA.class "pa3 vs3" ]
            [ viewSvgAnimation m
            ]
        ]


viewAlert m =
    H.div [ HA.classList [ ( "absolute w-100 pe-none", True ), ( "dn", m.connected ) ] ]
        [ H.div [ HA.class "relative flex flex-row justify-center w-100 " ]
            [ H.div
                [ HA.class
                    "relative top--1 shadow-1 f7 code bg-gold black pb1 pt3 br2 ph3 tc pe-bb"
                ]
                [ H.text "Disconnected from dev server"
                ]
            ]
        ]


viewSvgAnimation : Model -> Html Msg
viewSvgAnimation m =
    let
        viewControls =
            H.div [ HA.class "hs2 no-sel" ]
                [ hBtn [] Reset "Reset"
                , hBtn [] Restart "Restart"
                , hBtn [] TogglePause (ter (isPaused m) "Resume" "Pause")
                , hBtn [ HA.disabled (isRunning m) ] StepBack "<"
                , hBtn [ HA.disabled (isRunning m) ] StepForward ">"
                ]

        { warpBall, balls, sun, planet, ship, shipAngle } =
            case m.simulation.status of
                Paused n ->
                    m.simulation.history
                        |> List.Extra.getAt n
                        |> Maybe.withDefault (getCurrentSnapshot m)

                Running ->
                    getCurrentSnapshot m

        viewContent =
            H.div [ HA.class "no-sel", HE.onDoubleClick Restart ]
                [ Svg.svg
                    [ HA.class "flex center"
                    , HA.width worldWidth
                    , HA.height worldHeight
                    , HE.onBlur Resume
                    ]
                    (ViewSvg.view worldSize
                        [ ViewSvg.viewParticle warpBall "pink"
                        , ViewSvg.viewBalls balls
                        , ViewSvg.viewParticle sun "orange"
                        , ViewSvg.viewParticle planet "red"
                        , ViewSvg.viewShip ship shipAngle (isThrusting m)
                        ]
                    )
                ]

        viewStats stats =
            let
                statsStr =
                    String.Conversions.fromRecord
                        [ ( "ballCount", .ballCount >> String.fromInt )
                        , ( "ship", .ship >> partStr )
                        ]
                        stats

                strFromVec2 =
                    V.toRecord
                        >> String.Conversions.fromRecord
                            [ ( "x", .x >> Round.round 0 )
                            , ( "y", .y >> Round.round 0 )
                            ]

                partStr : P.Particle -> String
                partStr =
                    P.toRec
                        >> String.Conversions.fromRecord
                            [ ( "pos", .pos >> strFromVec2 )
                            , ( "vel", .vel >> strFromVec2 )
                            ]

                connectedStats =
                    m
                        |> String.Conversions.fromRecord
                            [ ( "connected", .connected >> String.Conversions.fromBool ) ]
            in
            H.div [ HA.class "code" ]
                [ H.div [ HA.class "" ] [ H.text statsStr ]
                , H.div [ HA.class "" ] [ H.text connectedStats ]
                ]
    in
    H.div [ HA.class "vs3" ]
        [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
        , viewControls
        , viewStats m.stats
        , viewContent
        ]



---- Subscriptions ----


subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AFrame
        , Browser.Events.onKeyDown (D.field "key" D.string |> D.map KeyDown)
        , Browser.Events.onKeyUp (D.field "key" D.string |> D.map KeyUp)
        , Time.every 1000 (\_ -> EverySecond)
        , Time.every 5000 (\_ -> Every5Second)

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
