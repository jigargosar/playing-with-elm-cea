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
import Render


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
    PairA.toFloat cellXY
        |> PairA.map (\fromTo -> createMonsterAnim clock fromTo fromTo)
        |> R.uncurry Monster


type alias Monsters =
    List Monster


type alias PressedKeys =
    List Keyboard.Key


type alias Model =
    { gridSize : Int2
    , playerXa : Animation
    , playerYa : Animation
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


initPlayerXYa clock =
    ( createAnim (xCells / 2) (xCells / 2) clock
    , createAnim (yCells / 2) (yCells / 2) clock
    )


gridSizeI2 : Int2
gridSizeI2 =
    ( xCells, yCells )


gridSizeF2 : Float2
gridSizeF2 =
    gridSizeI2 |> PairA.toFloat


xCells =
    2 * 9


yCells =
    2 * 8


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

        ( defaultPlayerXa, defaultPlayerYa ) =
            initPlayerXYa 0
    in
        { gridSize = gridSizeI2
        , playerXa = defaultPlayerXa
        , playerYa = defaultPlayerYa
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


animRetargetToI =
    toFloat >> animRetargetTo



--animToGridCellPx clock anim =
--    (A.animate clock anim) * cellSize
--


getPlayerXYpx : Model -> ( Float, Float )
getPlayerXYpx m =
    ( m.playerXa, m.playerYa )
        |> R.mapBothWith (A.animate m.clock)


getPortalXYpx : Model -> Float2
getPortalXYpx m =
    m.portal |> PairA.toFloat


getMonsterXYpx : A.Clock -> Monster -> Float2
getMonsterXYpx clock mon =
    ( mon.xa, mon.ya )
        |> PairA.map (A.animate clock)


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


computeNewClock : Time.Posix -> Model -> A.Clock
computeNewClock time m =
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


xyDiameterExtrema ( x, y ) dia =
    Extrema x (x + dia) y (y + dia)


xyBB xy =
    xyDiameterExtrema xy (Render.defaultR * 2) |> BoundingBox2d.fromExtrema


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



--defaultDia =
--    (cellSize - wallThickness) - (cellSize / 5)
--
--
--defaultRadius =
--    defaultDia / 2
