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
    = NewGame
    | Running
    | LevelComplete
    | NextLevel
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
    { playerXa : Animation
    , playerYa : Animation
    , pressedKeys : PressedKeys
    , pageLoadedAt : Int
    , clock : A.Clock
    , level : Int
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


type alias Player =
    { xa : Animation, ya : Animation }


initPlayer : Clock -> Player
initPlayer clock =
    Player (createAnim (xCells / 2) (xCells / 2) clock)
        (createAnim (yCells / 2) (yCells / 2) clock)


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


maxGridXY : Int2
maxGridXY =
    gridSizeI2 |> R.mapBothWith ((+) -1)


gridCellXYGenerator : Random.Generator Int2
gridCellXYGenerator =
    maxGridXY
        |> PairA.map (Random.int 0)
        |> R.uncurry Random.pair


monsterGenerator : Clock -> Random.Generator Monster
monsterGenerator clock =
    gridCellXYGenerator |> Random.map (initMonster clock)


monstersGenerator : Int -> Clock -> Random.Generator Monsters
monstersGenerator ct =
    monsterGenerator >> Random.list ct


type alias Level =
    { level : Int, player : Player, maze : Maze, monsters : Monsters, portal : Portal }


nextLevelGenerator : Model -> Random.Generator Level
nextLevelGenerator m =
    let
        nextLevel =
            m.level + 1
    in
        levelGenerator (m.level + 1) (m.clock)


levelGenerator : Int -> Clock -> Random.Generator Level
levelGenerator level clock =
    Random.map3 (Level level (initPlayer clock))
        (Maze.generator gridSizeI2)
        (monstersGenerator level clock)
        portalGenerator


portalGenerator : Random.Generator Portal
portalGenerator =
    gridCellXYGenerator


init : Flags -> Model
init { now } =
    let
        initialSeed =
            Random.initialSeed now

        ( { maze, level, monsters, player }, modelSeed ) =
            Random.step (levelGenerator 1 0) initialSeed
    in
        { playerXa = player.xa
        , playerYa = player.ya
        , pressedKeys = []
        , pageLoadedAt = now
        , clock = 0
        , level = level
        , maze = maze
        , monsters = monsters
        , game = Running
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


getPlayerXY : Model -> ( Float, Float )
getPlayerXY m =
    ( m.playerXa, m.playerYa )
        |> PairA.map (A.animate m.clock)


getPortalXY : Model -> Float2
getPortalXY m =
    m.portal |> PairA.toFloat


getMonsterXY : A.Clock -> Monster -> Float2
getMonsterXY clock mon =
    ( mon.xa, mon.ya )
        |> PairA.map (A.animate clock)


clampGridX : Model -> F Int
clampGridX m x =
    let
        ( w, _ ) =
            gridSizeI2
    in
        clamp 0 (w - 1) x


clampGridY : Model -> F Int
clampGridY m y =
    let
        ( _, h ) =
            gridSizeI2
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
    getPlayerXY >> xyBB


portalBB =
    getPortalXY >> xyBB


isGameOver m =
    m.monsters |> List.any (monsterBB m >> playerIntersects m)


isLevelComplete m =
    portalBB m |> playerIntersects m


playerIntersects m =
    BoundingBox2d.intersects (playerBB m)


monsterBB m mon =
    xyBB (getMonsterXY m.clock mon)
