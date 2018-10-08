module Model exposing (..)

import Animation as A exposing (Animation, Clock)
import BoundingBox2d
import Keyboard
import Keyboard.Arrows
import List.Extra
import Maze exposing (Maze)
import Ramda as R exposing (F)
import Random
import Time
import Set exposing (Set)
import MazeGenerator as MG exposing (MazeGenerator)
import PairA exposing (Float2, Int2, PairA)


---- MODEL ----


type alias Portal =
    Int2


type Game
    = Init
    | Running
    | LevelComplete
    | Over


type alias Monster =
    { xa : Animation, ya : Animation }


initMonster : Clock -> Int2 -> Monster
initMonster clock cellXY =
    PairA.floatFromInt cellXY
        |> PairA.map (\fromTo -> createMonsterAnim clock fromTo fromTo)
        |> R.uncurry Monster


type alias Monsters =
    List Monster


type alias PressedKeys =
    List Keyboard.Key


type alias Model =
    { gridSize : Int2
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


createMonsterAnim clock from to =
    A.animation clock
        |> A.from from
        |> A.to to
        |> A.ease identity
        |> A.speed 0.003


defaultPlayerXYa =
    createAnim 1 1 0


gridSizeI2 : Int2
gridSizeI2 =
    ( xCells, yCells )


gridSizeF2 : Float2
gridSizeF2 =
    gridSizeI2 |> PairA.floatFromInt


xCells =
    18


yCells =
    16


cellSize : Float
cellSize =
    26


wallThickness : Float
wallThickness =
    cellSize / 10


init : Flags -> Model
init { now } =
    let
        initialSeed =
            Random.initialSeed now

        ( mazeSeed, modelSeed ) =
            Random.step Random.independentSeed initialSeed
    in
        { gridSize = gridSizeI2
        , pxAnim = defaultPlayerXYa
        , pyAnim = defaultPlayerXYa
        , pressedKeys = []
        , pageLoadedAt = now
        , clock = 0
        , maze = Maze.init mazeSeed gridSizeI2
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


getPlayerXYpx : Model -> ( Float, Float )
getPlayerXYpx m =
    ( m.pxAnim, m.pyAnim )
        |> R.mapBothWith (animToGridCellPx m.clock)


getPortalXYpx : Model -> Float2
getPortalXYpx m =
    m.portal |> PairA.floatFromInt |> PairA.mul cellSize


getMonsterXYpx : A.Clock -> Monster -> Float2
getMonsterXYpx clock mon =
    ( mon.xa, mon.ya )
        |> R.mapBothWith (animToGridCellPx clock)


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
    Extrema x (x + defaultDia) y (y + defaultDia)


xyBB xy =
    xyDiameterExtrema xy defaultDia |> BoundingBox2d.fromExtrema


playerBB =
    getPlayerXYpx >> xyBB


portalBB =
    getPortalXYpx >> xyBB


isGameOver m =
    m.monsters |> List.any (monsterBB m >> playerIntersects m)


isLevelComplete m =
    portalBB m |> playerIntersects m


playerIntersects m =
    BoundingBox2d.intersects (playerBB m)


monsterBB m mon =
    xyBB (getMonsterXYpx m.clock mon)


defaultDia =
    (cellSize - wallThickness) - (cellSize / 40)


defaultRadius =
    defaultDia / 2
