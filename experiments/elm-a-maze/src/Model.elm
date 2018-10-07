module Model exposing (..)

import Animation as A exposing (Animation, Clock)
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


---- MODEL ----


type Game
    = Logo
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
    , seed : Random.Seed
    , pxAnim : Animation
    , pyAnim : Animation
    , pressedKeys : PressedKeys
    , pageLoadedAt : Int
    , clock : A.Clock
    , maze : Maze
    , monsters : Monsters
    , game : Game
    }


type alias Flags =
    { now : Int }


createAnim from to { clock } =
    A.animation clock
        |> A.from from
        |> A.to to
        |> A.ease identity
        |> A.speed 0.003


createMonsterAnim from to { clock } =
    A.animation clock
        |> A.from from
        |> A.to to
        |> A.ease identity
        |> A.speed 0.003


defaultAnim =
    createAnim 1 1 { clock = 0 }


gridSize : IntPair
gridSize =
    ( xCells, yCells )


xCells =
    18


yCells =
    12


cellSize =
    32


init : Flags -> Model
init { now } =
    let
        initialSeed =
            Random.initialSeed 1

        ( mazeSeed, modelSeed ) =
            Random.step Random.independentSeed initialSeed
    in
        { gridSize = gridSize
        , seed = modelSeed
        , pxAnim = defaultAnim
        , pyAnim = defaultAnim
        , pressedKeys = []
        , pageLoadedAt = now
        , clock = 0
        , maze = Maze.init mazeSeed gridSize
        , monsters = []
        , game = Logo
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
    { pressedKeys : String }


getDebugState : Model -> DebugModel
getDebugState m =
    { pressedKeys = m.pressedKeys |> Debug.toString }


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
