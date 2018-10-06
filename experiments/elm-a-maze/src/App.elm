port module App exposing (..)

import Animation as A exposing (Animation, Clock)
import Browser.Dom
import Browser.Dom as BD
import Browser.Dom as B
import Color exposing (Color)
import IntPair as IP exposing (IntPair)
import Html as H exposing (Html)
import Html.Lazy as H
import Html.Attributes as H
import Html.Attributes as HA
import Keyboard
import Keyboard.Arrows
import Light
import List.Extra
import Maybe.Extra
import Maze exposing (Maze)
import PairA
import Ramda as R
import Random
import Size
import Svg
import Svg as S
import Svg.Attributes as S
import Svg.Attributes as SA
import Svg.Keyed
import Time
import TypedSvg as T
import TypedSvg.Attributes as TA
import TypedSvg.Attributes.InPx as T
import TypedSvg.Attributes.InPx as TP
import Browser
import Browser as B
import Browser.Events as B
import Browser.Events as BE
import Set exposing (Set)
import Json.Decode as D
import Json.Encode as E
import TypedSvg.Types exposing (CoordinateSystem(..), Fill(..), Transform(..), percent, px)
import MazeGenerator as MG exposing (MazeGenerator)
import ISvg exposing (iCX, iCY, iFontSize, iHeight, iR, iStrokeWidth, iTranslate, iTranslateCord, iViewBox, iWidth, iX, iX1, iX2, iY, iY1, iY2)
import Svg.Lazy
import Svg.Lazy as S
import Update.Extra


---- PORTS ----


port onWindowBlur : (() -> msg) -> Sub msg


port onAnimationFrame : (() -> msg) -> Sub msg



---- MODEL ----


type MovementAxis
    = Horizontal
    | Vertical


type alias Movement =
    { axis : MovementAxis, to : Int, current : Float }


type alias Monster =
    { xa : Animation, ya : Animation }


type alias Model =
    { gridSize : IntPair
    , seed : Random.Seed
    , pxAnim : Animation
    , pyAnim : Animation
    , pressedKeys : List Keyboard.Key
    , pageLoadedAt : Int
    , clock : A.Clock
    , maze : Maze
    , monsters : List Monster
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
    ( 18, 12 )


cellSize =
    32


init : Flags -> ( Model, Cmd Msg )
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
        }
            |> pure


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



{-
   |> \xy ->
       case xy of
           ( 0, _ ) ->
               xy

           ( _, 0 ) ->
               xy

           ( x, y ) ->
               getFirstArrowKey m
                   |> Maybe.map (\k -> R.ter (isXArrowKey k) ( x, 0 ) ( 0, y ))
                   |> Maybe.withDefault ( x, y )
                   |> Debug.log "getArrows"
-}


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



---- UPDATE ----


type Msg
    = NoOp
    | OnWindowBlur ()
    | KeyMsg Keyboard.Msg
    | AnimationFrame Time.Posix
    | AnimationFramePort ()
    | UpdatePlayer Clock
    | UpdateMonsters Clock
    | GenerateMonsters


pure model =
    ( model, Cmd.none )


addCmd c2 =
    Tuple.mapSecond (\c1 -> Cmd.batch [ c1, c2 ])


withCmd c m =
    pure m |> addCmd c


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        AnimationFramePort _ ->
            pure m

        OnWindowBlur _ ->
            {- pure { m | pressedKeys = [] } -}
            pure m

        KeyMsg keyMsg ->
            {- pure m -}
            pure { m | pressedKeys = Keyboard.update keyMsg m.pressedKeys }

        UpdatePlayer clock ->
            let
                ( newPxAnim, newPyAnim ) =
                    computeNewXYAnim m
            in
                pure { m | pxAnim = newPxAnim, pyAnim = newPyAnim }

        {- pure m -}
        GenerateMonsters ->
            let
                ( newMonsters, newSeed ) =
                    generateMonsters m
            in
                pure { m | monsters = newMonsters, seed = newSeed }

        UpdateMonsters clock ->
            let
                newMonsters =
                    m.monsters |> List.map (updateMonster m)
            in
                pure { m | monsters = newMonsters }

        {- pure m -}
        AnimationFrame posix ->
            let
                newClock =
                    getClock posix m
            in
                { m | clock = newClock, gridSize = gridSize }
                    |> pure
                    |> Update.Extra.filter (noMonsters m)
                        (Update.Extra.andThen update GenerateMonsters)
                    |> Update.Extra.sequence update [ UpdatePlayer newClock, UpdateMonsters newClock ]


type alias F a =
    a -> a


getMonsterXInt { xa } =
    A.getTo xa |> round


getMonsterYInt { ya } =
    A.getTo ya |> round


computeMonsterNewX : Int -> Model -> Monster -> Maybe Float
computeMonsterNewX offset m monster =
    let
        isConnected cp =
            Maze.connected cp m.maze

        currentY =
            getMonsterYInt monster

        oldX =
            getMonsterXInt monster

        newX =
            (getMonsterXInt monster + offset)
                |> clampGridX m

        canMove =
            oldX /= newX && isConnected ( ( oldX, currentY ), ( newX, currentY ) )
    in
        (if canMove then
            toFloat newX |> Just
         else
            Nothing
        )


computeMonsterNewY : Int -> Model -> Monster -> Maybe Float
computeMonsterNewY offset m monster =
    let
        isConnected cp =
            Maze.connected cp m.maze

        currentX =
            getMonsterXInt monster

        oldY =
            getMonsterYInt monster

        newY =
            (oldY + offset)
                |> clampGridY m

        canMove =
            oldY /= newY && isConnected ( ( currentX, oldY ), ( currentX, newY ) )
    in
        (if canMove then
            toFloat newY |> Just
         else
            Nothing
        )


getAnimDir anim =
    A.getTo anim - A.getFrom anim |> R.sign |> R.when (R.equals 0) (always 1) |> round


computeMonsterNewXa : Model -> Monster -> Animation
computeMonsterNewXa m monster =
    let
        xa =
            monster.xa

        xDir =
            getAnimDir xa
    in
        computeMonsterNewX xDir m monster
            |> Maybe.Extra.orElseLazy (\_ -> computeMonsterNewX -xDir m monster)
            |> Maybe.map (\newTo -> animRetargetTo newTo m xa)
            |> Maybe.withDefault xa


computeMonsterNewYa : Model -> Monster -> Animation
computeMonsterNewYa m monster =
    let
        ya =
            monster.ya

        yDir =
            getAnimDir ya
    in
        computeMonsterNewY yDir m monster
            |> Maybe.Extra.orElseLazy (\_ -> computeMonsterNewY -yDir m monster)
            |> Maybe.map (\newTo -> animRetargetTo newTo m ya)
            |> Maybe.withDefault ya


updateMonster : Model -> F Monster
updateMonster m monster =
    let
        xa =
            monster.xa

        ya =
            monster.ya

        isMoving =
            isRunning xa m || isRunning ya m
    in
        (if isMoving then
            monster
         else
            let
                newXa =
                    computeMonsterNewXa m monster

                newYa =
                    computeMonsterNewYa m monster
            in
                (if Basics.modBy 10 (round m.clock) > 5 then
                    { monster | xa = newXa }
                 else
                    { monster | ya = newYa }
                )
        )


getMaxGridXY : Model -> IntPair
getMaxGridXY =
    .gridSize >> R.mapBothWith ((+) -1)


gridCordGenerator : Model -> Random.Generator IntPair
gridCordGenerator m =
    let
        ( maxX, maxY ) =
            getMaxGridXY m
    in
        Random.map2 Tuple.pair (Random.int 0 maxX) (Random.int 0 maxY)


monsterGenerator : Model -> Random.Generator Monster
monsterGenerator m =
    gridCordGenerator m
        |> Random.map (R.mapBothWith (toFloat >> \c -> createMonsterAnim c c m))
        |> Random.map (\( xa, ya ) -> Monster xa ya)


generateMonsters : Model -> ( List Monster, Random.Seed )
generateMonsters m =
    Random.step (monsterGenerator m |> Random.list 10) m.seed


computeNewXYAnim m =
    getFirstArrowKey m
        |> Maybe.map
            (\key ->
                let
                    isConnected cp =
                        Maze.connected cp m.maze

                    ( dx, dy ) =
                        getArrows m

                    ( xCells, yCells ) =
                        gridSize

                    newPxAnim =
                        if dx /= 0 && notRunning m.pyAnim m then
                            computeNewAnim
                                (\xx ->
                                    let
                                        ( x1, x2 ) =
                                            xx |> R.mapBothWith round

                                        y =
                                            A.getTo m.pyAnim |> round
                                    in
                                        isConnected ( ( x1, y ), ( x2, y ) )
                                )
                                xCells
                                dx
                                m.pxAnim
                                m
                        else
                            m.pxAnim

                    newPyAnim =
                        if dy /= 0 && notRunning m.pxAnim m then
                            computeNewAnim
                                (\yy ->
                                    let
                                        ( y1, y2 ) =
                                            yy |> R.mapBothWith round

                                        x =
                                            A.getTo m.pxAnim |> round
                                    in
                                        isConnected ( ( x, y1 ), ( x, y2 ) )
                                )
                                yCells
                                dy
                                m.pyAnim
                                m
                        else
                            m.pyAnim

                    yChanged =
                        A.equals newPyAnim m.pyAnim |> not

                    xChanged =
                        A.equals newPxAnim m.pxAnim |> not
                in
                    if xChanged && (isXArrowKey key || not yChanged) then
                        ( newPxAnim, m.pyAnim )
                    else if yChanged && (isYArrowKey key || not xChanged) then
                        ( m.pxAnim, newPyAnim )
                    else
                        ( m.pxAnim, m.pyAnim )
            )
        |> Maybe.withDefault ( m.pxAnim, m.pyAnim )


computeNewAnim isConnected cellCount dd anim m =
    let
        newDirection =
            toFloat dd

        current =
            (animCurrent anim m)

        from =
            A.getFrom anim

        to =
            A.getTo anim

        diff =
            from - to |> abs

        currentDirection =
            -from + to |> R.sign

        directionReversed =
            currentDirection /= 0 && newDirection /= 0 && currentDirection == newDirection * -1

        travelled =
            current - to |> abs

        clampTo =
            clamp 0 ((toFloat cellCount) - 1)

        newTo =
            to
                + newDirection
                |> clampTo
    in
        if (notRunning anim m || directionReversed) && isConnected ( to, newTo ) && to /= newTo then
            createAnim current newTo m
        else
            anim



---- VIEW ----


type alias View =
    Html Msg


canvasSizePair : IntPair
canvasSizePair =
    gridSize |> PairA.add 2 >> PairA.mul cellSize


canvasSizeRec =
    canvasSizePair |> PairA.toWhRec


canvasWHStyles =
    canvasSizePair
        |> PairA.fromIntWithSuffix "px"
        |> Tuple.mapBoth (H.style "min-width") (H.style "min-height")
        |> R.tupleToList


divClass class =
    divClassA class []


divClassA class attrs =
    H.div ([ H.class class ] ++ attrs)


el attrs child =
    H.div attrs [ child ]


view : Model -> View
view m =
    divClassA "flex flex-column items-center pa2 h-100"
        canvasWHStyles
        [ divClass "flex flex-column vs3 h-100"
            [ divClass "f3 tc" [ H.text "A-Maze-Zing!" ]
            , divClass "flex-auto overflow-scroll" [ viewSvg m ]
            , divClass "f7" [ viewDebug (getDebugState m) ]
            ]
        ]


viewDebug : DebugModel -> View
viewDebug { pressedKeys } =
    H.div [] [ H.text pressedKeys ]


svgAttrs =
    [ iViewBox -cellSize -cellSize canvasSizeRec.w canvasSizeRec.h ] ++ canvasWHStyles


viewSvg : Model -> View
viewSvg m =
    S.svg svgAttrs
        ([ S.lazy svgDefs (), S.lazy bkgRect (), viewGameContent m |> Svg.g [] ] {- ++ viewGameContent m -})


cellCenterF =
    (cellSize - wallThicknessF) / 2


svgDefs _ =
    let
        offset =
            5

        radius =
            (cellSize - wallThicknessF - offset) / 2

        x =
            cellCenterF

        y =
            cellCenterF

        cXYAttrs =
            ( x, y )
                |> Tuple.mapBoth TP.cx TP.cy
                |> R.tupleToList

        rAttr =
            TP.r radius
    in
        S.defs []
            [ monsterDef
            , playerDef
            ]


monsterDef =
    let
        offset =
            5

        radius =
            (cellSize - wallThicknessF - offset) / 2 |> round

        ( x, y ) =
            cellCenterF
                |> R.toTuple
                |> R.mapBothWith round
    in
        S.circle
            ([ iCX x
             , iCY y
             , iR radius
             , Color.darkOrange |> fillColor
             , S.id "monster"
             , TA.primitiveUnits CoordinateSystemUserSpaceOnUse
             ]
            )
            []


playerDef =
    let
        offset =
            5

        radius =
            (cellSize - wallThicknessF - offset) / 2 |> round

        cXYAttrs =
            cellCenterF
                |> R.toTuple
                |> R.mapBothWith (round)
                |> Tuple.mapBoth iCX iCY
                |> R.tupleToList

        rAttr =
            iR radius
    in
        S.circle
            (cXYAttrs
                ++ [ rAttr
                   , Color.green |> Light.map (\h -> { h | s = 1, l = 0.8 }) |> fillColor
                   , S.id "player"
                   , TA.primitiveUnits CoordinateSystemUserSpaceOnUse
                   ]
            )
            []


bkgRect _ =
    S.rect
        [ S.width "200%"
        , S.height "200%"
        , S.x "-100%"
        , S.y "-100%"
        , TA.strokeWidth (px 0.2)
        , TA.stroke Color.black
        , Color.blue
            |> Light.map (\h -> { h | s = 0.7, l = 0.7 })
            |> fillColor
        ]
        []


viewGameContent : Model -> List View
viewGameContent m =
    ([ S.lazy viewGridCells m.gridSize
     , S.lazy viewMazeWalls m.maze

     {- , viewPlayer (getPlayerCellXY m) -}
     , viewMonsters m.clock m.monsters |> Svg.Keyed.node "g" []
     ]
     {- ++ viewMonsters m.clock m.monsters -}
    )


wallThickness =
    cellSize // 10


wallThicknessF =
    toFloat wallThickness


viewMazeWalls : Maze -> View
viewMazeWalls maze =
    let
        cellSizePx =
            cellSize

        isConnected cp =
            Maze.connected cp maze

        isSouthConnected ( x, y ) =
            isConnected ( ( x, y ), ( x, y + 1 ) )

        isEastConnected ( x, y ) =
            isConnected ( ( x, y ), ( x + 1, y ) )

        size =
            cellSizePx

        viewCell cord =
            let
                ( x, y ) =
                    IP.scale size cord

                eastWall =
                    Svg.rect
                        [ iX (x + size - wallThickness)
                        , iY y
                        , iWidth wallThickness
                        , iHeight size
                        , SA.fill "#000"

                        --                        , R.ter (isEastConnected cord) "0" "1" |> SA.opacity
                        ]
                        []

                southWall =
                    Svg.rect
                        [ iX x
                        , iY (y + size - wallThickness)
                        , iWidth size
                        , iHeight wallThickness
                        , SA.fill "#000"

                        --                        , R.ter (isSouthConnected cord) "0" "1" |> SA.opacity
                        ]
                        []
            in
                R.ter (isEastConnected cord) [] [ eastWall ]
                    ++ R.ter (isSouthConnected cord) [] [ southWall ]
    in
        Maze.concatMapCells viewCell maze |> List.concat |> S.g []


viewPlayer ( x, y ) =
    S.lazy2 viewPlayerXY (round x) (round y)


viewPlayerXY x y =
    S.use ([ iX x, iY y, S.xlinkHref "#player" ]) []



{- S.circle
   (cXYAttrs ++ [ rAttr, Color.green |> Light.map (\h -> { h | s = 1, l = 0.89 }) |> fillColor ])
   []
-}


viewMonsters clock =
    List.indexedMap (\idx mon -> ( String.fromInt idx, mon |> getMonsterCellXY clock >> viewMonster ))


viewMonster ( x, y ) =
    S.lazy2
        viewMonsterHelp
        x
        y


centerOffset =
    let
        offset =
            5

        radius =
            (cellSize - wallThicknessF - offset) / 2 |> round
    in
        cellCenterF
            |> R.toTuple
            |> R.mapBothWith round


centerOffsetF =
    centerOffset |> R.mapBothWith toFloat


monsRad =
    let
        offset =
            5
    in
        (cellSize - wallThicknessF - offset) / 2 |> round |> iR


mC =
    Color.darkOrange |> fillColor


centerOffsetX =
    centerOffset |> Tuple.first


centerOffsetY =
    centerOffset |> Tuple.second


centerOffsetXF =
    centerOffsetF |> Tuple.first


centerOffsetYF =
    centerOffsetF |> Tuple.second


centerOffsetTransformF =
    TA.transform [ Translate centerOffsetXF centerOffsetYF ]


centerOffsetTransform =
    S.transform (iTranslate centerOffsetX centerOffsetY)


viewMonsterHelp x y =
    S.circle
        ([ mC
         , monsRad
         , iCX (x + centerOffsetX)
         , iCY (y + centerOffsetY)

         {- ,  iCX (x)
            , iCY (y)
            , centerOffsetTransform
         -}
         ]
        )
        []



--    S.use ([ iX x, iY y, S.xlinkHref "#monster" ]) []
--    S.use ([ {- iTranslate x y |> S.transform, -} iX x, iY y, S.xlinkHref "#monster" ]) []


viewGridCells size =
    gridConcatMap size viewGridCell |> S.g [ opacityFloat 0.05 ]


cordToPx =
    R.mapBothWith (toFloat >> (*) cellSize)


viewGridCell cord =
    let
        xyAttr =
            cord |> cordToPx |> R.mapBothWith ((+) (-wallThicknessF / 2)) |> Tuple.mapBoth TP.x TP.y |> R.tupleToList

        whAttr =
            px cellSize |> \s -> ( s, s ) |> Tuple.mapBoth TA.width TA.height |> R.tupleToList
    in
        S.rect (xyAttr ++ whAttr ++ [ strokeColor Color.black, TP.strokeWidth 1, TA.noFill ]) []


gridConcatMap size fn =
    gridMap size fn |> List.concat


gridMap ( width, height ) fn =
    let
        xCords =
            List.range 0 (width - 1)

        yCords =
            List.range 0 (height - 1)
    in
        yCords
            |> List.map
                (\y ->
                    xCords
                        |> List.map (\x -> fn ( x, y ))
                )



---- SVG ATTRIBUTES ----


type alias SvgAttribute msg =
    Svg.Attribute msg


fillColor : Color -> SvgAttribute msg
fillColor =
    Fill >> TA.fill


strokeColor : Color -> SvgAttribute msg
strokeColor =
    TA.stroke


fillOpacityFloat =
    TypedSvg.Types.Opacity >> TA.fillOpacity


opacityFloat =
    TypedSvg.Types.Opacity >> TA.opacity



---- PROGRAM ----


type alias Subs =
    Model -> Sub Msg


subscriptions : Subs
subscriptions _ =
    Sub.batch
        [ B.onAnimationFrame AnimationFrame
        , Sub.map KeyMsg Keyboard.subscriptions
        , onWindowBlur OnWindowBlur
        , onAnimationFrame AnimationFramePort
        ]


main : Program Flags Model Msg
main =
    B.element
        { view = H.lazy view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
