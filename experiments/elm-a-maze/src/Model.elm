module Model exposing (..)

import Animation as A exposing (Animation, Clock)
import BoundingBox2d
import IntPair as IP exposing (IntPair)
import Keyboard
import Keyboard.Arrows
import List.Extra
import Maze exposing (Maze)
import Ramda as R exposing (F)
import Random
import Time
import Set exposing (Set)
import MazeGenerator as MG exposing (MazeGenerator)
import PairA exposing (PairA)


---- MODEL ----


type alias Portal =
    IntPair


type Game
    = Init
    | Running
    | Paused
    | Over


type alias Monster =
    { xa : Animation, ya : Animation }


type alias Monsters =
    List Monster


type alias PressedKeys =
    List Keyboard.Key


type alias Model =
    { gridSize : IntPair
    , pxAnim : Animation
    , pyAnim : Animation
    , pressedKeys : PressedKeys
    , pageLoadedAt : Int
    , clock : A.Clock
    , maze : Maze
    , monsters : Monsters
    , game : Game
    , portal : Portal
    , seed : Random.Seed
    }


type alias Flags =
    { now : Int }


createAnim from to clock =
    A.animation clock
        |> A.from from
        |> A.to to
        |> A.ease identity
        |> A.speed 0.003


createMonsterAnim from to clock =
    A.animation clock
        |> A.from from
        |> A.to to
        |> A.ease identity
        |> A.speed 0.003


defaultPlayerXYa =
    createAnim 1 1 0


gridSize : IntPair
gridSize =
    ( xCells, yCells )


xCells =
    18


yCells =
    16


cellSize =
    26


wallThickness =
    cellSize // 10


wallThicknessF =
    toFloat wallThickness


init : Flags -> Model
init { now } =
    let
        initialSeed =
            Random.initialSeed now

        ( mazeSeed, modelSeed ) =
            Random.step Random.independentSeed initialSeed
    in
        { gridSize = gridSize
        , pxAnim = defaultPlayerXYa
        , pyAnim = defaultPlayerXYa
        , pressedKeys = []
        , pageLoadedAt = now
        , clock = 0
        , maze = Maze.init mazeSeed gridSize
        , monsters = []
        , game = Init
        , portal = PairA.zero
        , seed = modelSeed
        }


notRunning anim m =
    isScheduled anim m || isDone anim m


isRunning anim { clock } =
    A.isRunning clock anim


isScheduled anim m =
    A.isScheduled m.clock anim


isDone anim m =
    A.isDone m.clock anim


animCurrent anim m =
    A.animate m.clock anim


animRetargetTo to { clock } anim =
    A.retarget clock to anim


animToGridCellPx clock anim =
    (A.animate clock anim) * cellSize


getPlayerCellXY : Model -> ( Float, Float )
getPlayerCellXY m =
    ( m.pxAnim, m.pyAnim )
        |> R.mapBothWith (animToGridCellPx m.clock)


getMonsterCellXY : A.Clock -> Monster -> IntPair
getMonsterCellXY clock mon =
    ( mon.xa, mon.ya )
        |> R.mapBothWith (animToGridCellPx clock >> round)


clampGridX : Model -> F Int
clampGridX m x =
    let
        ( w, _ ) =
            m.gridSize
    in
        clamp 0 (w - 1) x


clampGridY : Model -> F Int
clampGridY m y =
    let
        ( _, h ) =
            m.gridSize
    in
        clamp 0 (h - 1) y


getClock : Time.Posix -> Model -> A.Clock
getClock time m =
    Time.posixToMillis time - m.pageLoadedAt |> toFloat


type alias DebugModel =
    { pressedKeys : String, game : String }


getDebugState : Model -> DebugModel
getDebugState m =
    { pressedKeys = m.pressedKeys |> Debug.toString, game = m.game |> Debug.toString }


getArrows m =
    Keyboard.Arrows.arrows m.pressedKeys
        |> (\{ x, y } -> ( x, -y ))


getArrowXDir =
    getArrows >> Tuple.first >> R.ifElse (R.equals 0) (\_ -> Nothing) Just


getArrowYDir =
    getArrows >> Tuple.second >> R.ifElse (R.equals 0) (\_ -> Nothing) Just


getFirstArrowKey : Model -> Maybe Keyboard.Key
getFirstArrowKey =
    .pressedKeys >> List.Extra.find isArrowKey


arrowXKeyList =
    [ Keyboard.ArrowLeft, Keyboard.ArrowRight ]


arrowYKeyList =
    [ Keyboard.ArrowUp, Keyboard.ArrowDown ]


arrowKeyList =
    arrowXKeyList ++ arrowYKeyList


isArrowKey : Keyboard.Key -> Bool
isArrowKey key =
    List.member key arrowKeyList


isXArrowKey key =
    List.member key arrowXKeyList


isYArrowKey key =
    List.member key arrowYKeyList


noMonsters =
    .monsters >> R.isListEmpty


type alias Extrema =
    { minX : Float, maxX : Float, minY : Float, maxY : Float }


extrema =
    Extrema 0 0 0 0


xyDiameterExtrema ( x, y ) diameter =
    Extrema x (x + playerDiameterF) y (y + playerDiameterF)


playerBoundingBox m =
    xyDiameterExtrema (getPlayerCellXY m) playerDiameterF |> BoundingBox2d.fromExtrema


isGameOver m =
    m.monsters
        |> List.any (monsterBoundingBox m >> BoundingBox2d.intersects (playerBoundingBox m))


monsterBoundingBox m mon =
    xyDiameterExtrema (getMonsterCellXY m.clock mon |> PairA.floatFromInt) monsterDiameterF |> BoundingBox2d.fromExtrema


monsterDiameterF =
    let
        offset =
            5
    in
        (cellSize - wallThicknessF - offset)


playerDiameterF =
    let
        offset =
            5
    in
        (cellSize - wallThicknessF - offset)


monsterRadius =
    monsterDiameterF / 2 |> round


playerRadius =
    playerDiameterF / 2 |> round
