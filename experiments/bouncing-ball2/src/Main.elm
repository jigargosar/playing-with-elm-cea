module Main exposing (main)

import Array
import Browser
import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy
import Json.Decode as D
import List.Extra
import Math.Vector2
import Particle exposing (Particle)
import Ramda exposing (subBy, ter)
import Random
import Round
import Set exposing (Set)
import String exposing (String)
import Svg
import Svg.Attributes as SA
import Time
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


initialShip =
    Particle.new { dp | va = 90, r = 50 }


dp =
    { x = 0, y = 0, vm = 0, va = 0, r = 10, am = 0, aa = 0, mass = 1 }


type alias Model =
    { paused : Bool
    , balls : List Ball
    , seed : Random.Seed
    , ship : Ship
    , shipAngle : Float
    , keyDownSet : Set String
    , sun : Particle
    , planet : Particle
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

        newBall vm va r =
            Particle.new
                { dp
                    | x = 0
                    , y = 100
                    , vm = vm
                    , va = va
                    , r = r
                    , am = 0.1
                    , aa = -90
                }
    in
    Random.map3 newBall magnitudeGenerator angleGenerator radiusGenerator


initialModel fromSeed =
    let
        ( balls, seed ) =
            Random.step (Random.list 500 ballGenerator) fromSeed

        sun =
            Particle.new { dp | r = 30, mass = 20000 }
    in
    { paused = False
    , balls = balls
    , seed = seed
    , ship = initialShip
    , shipAngle = 0
    , keyDownSet = Set.empty
    , sun = sun
    , planet = Particle.new { dp | x = 200, r = 5 }
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
    Math.Vector2.vec2 (toFloat worldWidth) (toFloat worldHeight)


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
            let
                angleOffset =
                    5
            in
            { m | planet = Particle.gravitateTo m.sun m.planet }
                |> updateParticles
                |> cond identity
                    [ ( isKeyDown "ArrowLeft", updateShipAngle ((+) -angleOffset) )
                    , ( isKeyDown "ArrowRight", updateShipAngle ((+) angleOffset) )
                    ]
                |> updateShipThrust
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
            let
                _ =
                    Debug.log "kD" key
            in
            pure { m | keyDownSet = Set.insert key m.keyDownSet }

        KeyUp key ->
            pure { m | keyDownSet = Set.remove key m.keyDownSet }


updateParticles m =
    { m
        | balls = m.balls |> List.map Particle.update
        , ship = m.ship |> Particle.update
        , planet = m.planet |> Particle.update
    }


cond defaultFn conditions data =
    conditions
        |> List.Extra.find (\( boolFn, _ ) -> boolFn data)
        |> Maybe.map (\( _, dataFn ) -> dataFn data)
        |> Maybe.withDefault (defaultFn data)


updateShipThrust m =
    case isKeyDown "ArrowUp" m of
        True ->
            { m | ship = m.ship |> Particle.setAccMA 0.1 m.shipAngle }

        False ->
            { m | ship = m.ship |> Particle.setAccMA 0 0 }


pure m =
    ( m, Cmd.none )


updateShipAngle fn m =
    { m | shipAngle = fn m.shipAngle }



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
                [ ViewSvg.viewBalls m.balls
                , ViewSvg.viewShip m.ship m.shipAngle

                {- , ViewSvg.viewParticle m.sun "orange"
                   , ViewSvg.viewParticle m.planet "red"
                -}
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
