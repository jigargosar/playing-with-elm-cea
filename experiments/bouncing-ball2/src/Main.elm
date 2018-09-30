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
import Ramda exposing (appendTo, flip, subBy, ter)
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
    , stats : { ballCount : Int, ship : Ship }
    , connected : Bool
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


initialModel fromSeed =
    let
        ( balls, seed ) =
            generateBalls 500 fromSeed

        sun =
            P.new { part | r = 20, mass = 1000 }

        initialShip =
            P.new { part | x = 200, vm = 2, va = 90, r = 50 }
    in
    { paused = False
    , balls = balls
    , seed = seed
    , ship = initialShip
    , shipAngle = 0
    , shipThrust = 0
    , keyDownSet = Set.empty
    , sun = sun
    , planet = P.new { part | y = 200, vm = 2, va = 0, r = 5 }
    , warpBall = P.new { part | x = 0, y = 0, vm = 5, va = 25, r = 50 }
    , stats = { ballCount = 0, ship = initialShip }
    , connected = True
    }
        |> updateStats


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
            (if m.paused then
                m

             else
                updateStats m
            )
                |> pure

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
            { m | paused = newPaused }
                |> updateStats
                |> pure

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
    H.div [ HA.class "absolute w-100 pe-none" ]
        [ H.div [ HA.class "relative flex flex-row justify-center w-100 " ]
            [ H.div
                [ HA.class
                    "relative top--1 shadow-1 f7 code bg-gold mid-gray pb1 pt3 br2 ph3 tc pe-bb"
                ]
                [ H.text "Disconnected from dev server"
                ]
            ]
        ]


viewSvgAnimation m =
    let
        viewControls =
            H.div [ HA.class "hs2 no-sel" ]
                [ hBtn [] Reset "Reset"
                , hBtn [] Restart "Restart"
                , hBtn [] TogglePause (ter m.paused "Play" "Pause")
                , hBtn [ HA.disabled (not m.paused) ] Step "Step"
                ]

        viewContent =
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
