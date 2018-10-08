port module App exposing (..)

import Animation as A exposing (Animation, Clock)
import Browser.Dom
import Browser.Dom as BD
import Browser.Dom as B
import Color exposing (Color)
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
import Model exposing (..)
import PairA exposing (IntPair)
import Ramda as R exposing (F)
import Random
import Random.Extra
import Return
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
import TypedSvg.Types
    exposing
        ( AlignmentBaseline(..)
        , AnchorAlignment(..)
        , CoordinateSystem(..)
        , Fill(..)
        , FontWeight(..)
        , Transform(..)
        , percent
        , px
        )
import MazeGenerator as MG exposing (MazeGenerator)
import ISvg
    exposing
        ( iCX
        , iCY
        , iFontSize
        , iHeight
        , iR
        , iStrokeWidth
        , iTranslate
        , iTranslateCord
        , iViewBox
        , iWidth
        , iX
        , iX1
        , iX2
        , iY
        , iY1
        , iY2
        )
import Svg.Lazy
import Svg.Lazy as S
import Update.Extra as Update exposing (filter)


---- PORTS ----


port onWindowBlur : (() -> msg) -> Sub msg


port onAnimationFrame : (() -> msg) -> Sub msg


init : Flags -> ( Model, Cmd Msg )
init =
    Model.init >> noCmd



---- UPDATE ----


type Msg
    = NoOp
    | OnWindowBlur ()
    | SetPressedKeys PressedKeys
    | SetMonsters Monsters
    | SetSeed Random.Seed
    | SetGame Game
    | KeyMsg Keyboard.Msg
    | AnimationFrame Time.Posix
    | AnimationFramePort ()
    | UpdatePlayer
    | UpdateMonsters
    | UpdateGame
    | UpdateGameOver
    | GenerateMonsters
    | PostInit


noCmd model =
    ( model, Cmd.none )


addCmd c2 =
    Tuple.mapSecond (\c1 -> Cmd.batch [ c1, c2 ])


withCmd c m =
    noCmd m |> addCmd c


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            noCmd m

        PostInit ->
            noCmd m

        AnimationFramePort _ ->
            noCmd m

        OnWindowBlur _ ->
            SetPressedKeys [] |> updateWithModel m

        SetPressedKeys newPressedKeys ->
            noCmd { m | pressedKeys = newPressedKeys }

        SetMonsters newMonsters ->
            noCmd { m | monsters = newMonsters }

        SetSeed newSeed ->
            noCmd { m | seed = newSeed }

        KeyMsg keyMsg ->
            Keyboard.update keyMsg m.pressedKeys |> SetPressedKeys |> updateWithModel m

        UpdatePlayer ->
            computeNewPlayerXYa m
                |> Maybe.Extra.unwrap m
                    (\( xa, ya ) -> { m | pxAnim = xa, pyAnim = ya })
                |> noCmd

        GenerateMonsters ->
            ( m, Random.generate SetMonsters (monstersGenerator 10 m) )

        UpdateMonsters ->
            ( m, Random.generate SetMonsters (monstersUpdateGenerator m) )

        SetGame v ->
            noCmd { m | game = v }

        UpdateGameOver ->
            noCmd m |> filter (isGameOver m) (sequence [ SetGame Model.Over ])

        UpdateGame ->
            noCmd m
                |> case m.game of
                    Model.Init ->
                        sequence [ GenerateMonsters, SetGame Model.Running ]

                    Model.Over ->
                        sequence [ UpdateMonsters ]

                    _ ->
                        sequence [ UpdatePlayer, UpdateMonsters, UpdateGameOver ]

        AnimationFrame posix ->
            let
                newClock =
                    getClock posix m
            in
                update UpdateGame { m | clock = newClock }


updateWithModel =
    R.flip update


sequence =
    Update.sequence update


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


monsterUpdateGenerator : Model -> Monster -> Random.Generator Monster
monsterUpdateGenerator m mon =
    Random.Extra.bool
        |> Random.map
            (\b ->
                if isRunning mon.xa m || isRunning mon.ya m then
                    mon
                else if b then
                    { mon | xa = computeMonsterNewXa m mon }
                else
                    { mon | ya = computeMonsterNewYa m mon }
            )


monstersUpdateGenerator : Model -> Random.Generator Monsters
monstersUpdateGenerator m =
    m.monsters |> List.map (monsterUpdateGenerator m) >> Random.Extra.combine


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


monstersGenerator : Int -> Model -> Random.Generator Monsters
monstersGenerator ct =
    monsterGenerator >> Random.list ct


areCellsConnected cp =
    .maze >> Maze.connected cp


computePlayerNewXa : Model -> Maybe Animation
computePlayerNewXa m =
    (if notRunning m.pyAnim m then
        getArrowXDir m
            |> Maybe.map
                (\dx ->
                    computeNewAnim
                        (\xx ->
                            let
                                ( x1, x2 ) =
                                    xx |> R.mapBothWith round

                                y =
                                    A.getTo m.pyAnim |> round
                            in
                                areCellsConnected ( ( x1, y ), ( x2, y ) ) m
                        )
                        xCells
                        dx
                        m.pxAnim
                        m
                )
     else
        Nothing
    )


computePlayerNewYa : Model -> Maybe Animation
computePlayerNewYa m =
    (if notRunning m.pxAnim m then
        getArrowYDir m
            |> Maybe.map
                (\dy ->
                    computeNewAnim
                        (\yy ->
                            let
                                ( y1, y2 ) =
                                    yy |> R.mapBothWith round

                                x =
                                    A.getTo m.pxAnim |> round
                            in
                                areCellsConnected ( ( x, y1 ), ( x, y2 ) ) m
                        )
                        yCells
                        dy
                        m.pyAnim
                        m
                )
     else
        Nothing
    )


computeNewPlayerXYa : Model -> Maybe ( Animation, Animation )
computeNewPlayerXYa m =
    getFirstArrowKey m
        |> Maybe.map
            (\key ->
                case ( computePlayerNewXa m, computePlayerNewYa m ) of
                    ( Just newXa, Nothing ) ->
                        ( newXa, m.pyAnim )

                    ( Nothing, Just newYa ) ->
                        ( m.pxAnim, newYa )

                    ( Nothing, Nothing ) ->
                        ( m.pxAnim, m.pyAnim )

                    ( Just newXa, Just newYa ) ->
                        (if isXArrowKey key then
                            ( newXa, m.pyAnim )
                         else
                            ( m.pxAnim, newYa )
                        )
            )


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
        if (notRunning anim m || directionReversed) && to /= newTo && isConnected ( to, newTo ) then
            createAnim current newTo m
        else
            anim



---- VIEW ----
---- HTML Wrappers----


divClass class =
    divClassA class []


divClassA class attrs =
    H.div ([ H.class class ] ++ attrs)


textClass class tc =
    divClass class [ H.text tc ]


text =
    textClass ""


type alias View =
    Html Msg


canvasSizePair : IntPair
canvasSizePair =
    gridSize |> PairA.add 2 >> PairA.mul cellSize


canvasSizeRec =
    canvasSizePair |> PairA.toWhRec


canvasCenter =
    canvasSizePair |> PairA.iDiv 2


canvasWHStyles =
    canvasSizePair
        |> PairA.fromIntWithSuffix "px"
        |> PairA.apply2 H.style ( "min-width", "min-height" )
        |> R.tupleToList


view : Model -> View
view m =
    divClassA "flex flex-column items-center pa2 h-100"
        canvasWHStyles
        [ divClass "flex flex-column vs3 h-100"
            [ divClass "f3 tc" [ H.text "A-Maze-Zing!" ]
            , divClass "flex1 overflow-scroll" [ viewSvg m ]
            , divClass "f7" [ viewDebug (getDebugState m) ]
            ]
        ]


viewDebug : DebugModel -> View
viewDebug { pressedKeys, game } =
    divClass "flex hs2" [ text pressedKeys, text game ]


svgAttrs =
    [ iViewBox -cellSize -cellSize canvasSizeRec.w canvasSizeRec.h
    , H.width canvasSizeRec.w
    , H.height canvasSizeRec.h
    ]


viewSvg : Model -> View
viewSvg m =
    S.svg svgAttrs
        ([ S.lazy bkgRect ()
         , viewGameContent m |> Svg.g []
         ]
        )


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
     , viewPlayer (getPlayerCellXY m)
     , viewMonsters m.clock m.monsters |> Svg.Keyed.node "g" []
     ]
        ++ viewGameOver m.game
    )


viewGameOver =
    let
        ( mw, mh ) =
            canvasSizePair

        h =
            mh // 4

        y =
            (mh - h) // 2

        rectAttrs =
            [ mw |> iWidth
            , h |> iHeight
            , fillColor Color.white
            , iY y
            ]

        render game =
            S.g
                [ iTranslate -cellSize (-cellSize) |> S.transform
                ]
                [ S.rect rectAttrs []
                , S.text_
                    [ fillColor Color.black
                    , strokeColor Color.black
                    , iFontSize 24
                    , FontWeightLighter |> TA.fontWeight

                    --                    , iStrokeWidth 1
                    , TA.textAnchor AnchorMiddle
                    , TA.alignmentBaseline AlignmentMiddle
                    , S.x "50%"
                    , S.y "50%"
                    ]
                    [ S.text "A-Maze-Zing! You Reached Level 1" ]
                ]
    in
        R.ifElse (R.equals Model.Over) (S.lazy render >> List.singleton) (always [])


viewMazeWalls : Maze -> View
viewMazeWalls maze =
    Maze.concatMapCells (viewWall maze) maze |> List.concat |> S.g []


viewWall maze cord =
    let
        ( x, y ) =
            PairA.mul cellSize cord

        eastWall =
            Svg.rect
                [ iX (x + cellSize - wallThickness)
                , iY y
                , iWidth wallThickness
                , iHeight cellSize
                , SA.fill "#000"
                ]
                []

        southWall =
            Svg.rect
                [ iX x
                , iY (y + cellSize - wallThickness)
                , iWidth cellSize
                , iHeight wallThickness
                , SA.fill "#000"
                ]
                []
    in
        R.ter (Maze.isEastConnected cord maze) [] [ eastWall ]
            ++ R.ter (Maze.isSouthConnected cord maze) [] [ southWall ]


viewPlayer ( x, y ) =
    S.lazy2 viewPlayerHelp (round x) (round y)


viewPlayerHelp x y =
    S.circle
        [ iCX (x + centerOffset)
        , iCY (y + centerOffset)
        , iR playerRadius
        , Color.green |> Light.map (\h -> { h | s = 1, l = 0.89 }) |> fillColor
        ]
        []


viewMonsters clock =
    List.indexedMap (\idx mon -> ( String.fromInt idx, mon |> getMonsterCellXY clock >> viewMonster ))


viewMonster ( x, y ) =
    S.lazy2 viewMonsterHelp x y


centerOffset =
    (cellSize - wallThickness) // 2


viewMonsterHelp x y =
    S.circle
        [ Color.darkOrange |> fillColor
        , iR monsterRadius
        , iCX (x + centerOffset)
        , iCY (y + centerOffset)
        ]
        []


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
