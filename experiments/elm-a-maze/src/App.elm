port module App exposing (..)

import Animation as A exposing (Animation, Clock)
import Basics.Extra exposing (uncurry)
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
import Maybe.Extra as Maybe
import Maze exposing (Maze)
import Model exposing (..)
import PairA exposing (Float2, Int2, PairA)
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
import TypedSvg.Types exposing (..)
import MazeGenerator as MG exposing (MazeGenerator)
import ISvg exposing (..)
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
    | SetPlayer (PairA Animation)
    | SetSeed Random.Seed
    | SetGame Game
    | KeyMsg Keyboard.Msg
    | AnimationFrame Time.Posix
    | AnimationFramePort ()
    | UpdatePlayer
    | UpdateMonsters
    | UpdateGame
    | UpdateLevel
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

        SetPlayer ( xa, ya ) ->
            noCmd { m | playerXa = xa, playerYa = ya }

        SetSeed newSeed ->
            noCmd { m | seed = newSeed }

        KeyMsg keyMsg ->
            let
                ( newPressedKeys, keyDowned ) =
                    m.pressedKeys
                        |> Keyboard.updateWithKeyChange Keyboard.anyKey keyMsg
                        |> Tuple.mapSecond
                            (Maybe.unwrap False
                                (\kc ->
                                    case kc of
                                        Keyboard.KeyDown _ ->
                                            True

                                        _ ->
                                            False
                                )
                            )
                        |> Debug.log "keyDowned"
            in
                update (SetPressedKeys newPressedKeys) m
                    |> filter (isLevelComplete m && keyDowned)
                        (sequence [ SetGame Model.Init ])

        UpdatePlayer ->
            computeNewPlayerXYa m
                |> Maybe.unwrap (noCmd m) (SetPlayer >> updateWithModel m)

        GenerateMonsters ->
            ( m, Random.generate SetMonsters (monstersGenerator 10 m.clock) )

        UpdateMonsters ->
            ( m, Random.generate SetMonsters (monstersUpdateGenerator m) )

        SetGame v ->
            noCmd { m | game = v }

        UpdateLevel ->
            let
                newMsg =
                    if isLevelComplete m then
                        [ SetGame Model.LevelComplete ]
                    else if isGameOver m then
                        [ SetGame Model.Over ]
                    else
                        []
            in
                noCmd m |> sequence newMsg

        UpdateGame ->
            noCmd m
                |> case m.game of
                    Model.Init ->
                        sequence
                            [ SetPlayer (initPlayerXYa m.clock)
                            , GenerateMonsters
                            , SetGame Model.Running
                            ]

                    Model.Over ->
                        sequence [ UpdateMonsters ]

                    Model.LevelComplete ->
                        sequence []

                    Model.Running ->
                        sequence [ UpdatePlayer, UpdateMonsters, UpdateLevel ]

        AnimationFrame posix ->
            let
                newClock =
                    computeNewClock posix m
            in
                update UpdateGame { m | clock = newClock }


updateWithModel =
    R.flip update


sequence =
    Update.sequence update


andThen =
    Update.andThen update


getMonsterX { xa } =
    A.getTo xa |> round


getMonsterY { ya } =
    A.getTo ya |> round


computeMonsterNewX : Int -> Model -> Monster -> Maybe Int
computeMonsterNewX offset m monster =
    let
        isConnected cp =
            Maze.connected cp m.maze

        currentY =
            getMonsterY monster

        oldX =
            getMonsterX monster

        newX =
            (getMonsterX monster + offset)
                |> clampGridX m

        canMove =
            oldX /= newX && isConnected ( ( oldX, currentY ), ( newX, currentY ) )
    in
        (if canMove then
            Just newX
         else
            Nothing
        )


computeMonsterNewY : Int -> Model -> Monster -> Maybe Int
computeMonsterNewY offset m monster =
    let
        isConnected cp =
            Maze.connected cp m.maze

        currentX =
            getMonsterX monster

        oldY =
            getMonsterY monster

        newY =
            (oldY + offset)
                |> clampGridY m

        canMove =
            oldY /= newY && isConnected ( ( currentX, oldY ), ( currentX, newY ) )
    in
        (if canMove then
            Just newY
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
            |> Maybe.orElseLazy (\_ -> computeMonsterNewX -xDir m monster)
            |> Maybe.map (\newTo -> animRetargetToI newTo m xa)
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
            |> Maybe.orElseLazy (\_ -> computeMonsterNewY -yDir m monster)
            |> Maybe.map (\newTo -> animRetargetToI newTo m ya)
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


areCellsConnected cp =
    .maze >> Maze.connected cp


computePlayerNewXa : Model -> Maybe Animation
computePlayerNewXa m =
    (if notRunning m.playerYa m then
        getArrowXDir m
            |> Maybe.map
                (\dx ->
                    computeNewAnim
                        (\xx ->
                            let
                                ( x1, x2 ) =
                                    xx |> R.mapBothWith round

                                y =
                                    A.getTo m.playerYa |> round
                            in
                                areCellsConnected ( ( x1, y ), ( x2, y ) ) m
                        )
                        xCells
                        dx
                        m.playerXa
                        m
                )
     else
        Nothing
    )


computePlayerNewYa : Model -> Maybe Animation
computePlayerNewYa m =
    (if notRunning m.playerXa m then
        getArrowYDir m
            |> Maybe.map
                (\dy ->
                    computeNewAnim
                        (\yy ->
                            let
                                ( y1, y2 ) =
                                    yy |> R.mapBothWith round

                                x =
                                    A.getTo m.playerXa |> round
                            in
                                areCellsConnected ( ( x, y1 ), ( x, y2 ) ) m
                        )
                        yCells
                        dy
                        m.playerYa
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
                        ( newXa, m.playerYa )

                    ( Nothing, Just newYa ) ->
                        ( m.playerXa, newYa )

                    ( Nothing, Nothing ) ->
                        ( m.playerXa, m.playerYa )

                    ( Just newXa, Just newYa ) ->
                        (if isXArrowKey key then
                            ( newXa, m.playerYa )
                         else
                            ( m.playerXa, newYa )
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
            createAnim current newTo m.clock
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


canvasSizeF2 : Float2
canvasSizeF2 =
    gridSizeF2 |> PairA.add 2 >> PairA.mul cellSize


canvasWHStyles =
    canvasSizeF2
        |> PairA.round
        |> PairA.fromIntThenSuffix "px"
        |> PairA.map2 H.style ( "min-width", "min-height" )
        |> R.tupleToList


view : Model -> View
view m =
    divClassA "flex flex-column items-center pa2"
        canvasWHStyles
        [ divClass "flex flex-column vs3"
            [ divClass "f3 tc" [ H.text "A-Maze-Zing!" ]
            , divClass "flex1 overflow-scroll" [ viewSvg m ]
            , divClass "f7" [ viewDebug (getDebugState m) ]
            ]
        ]


viewSvg : Model -> View
viewSvg m =
    let
        ( cw, ch ) =
            canvasSizeF2 |> PairA.round
    in
        S.svg [ H.width cw, H.height ch ]
            ([ bkgRect
             , S.g [ TA.transform [ Translate cellSize cellSize ] ]
                [ S.lazy viewMazeWalls m.maze
                , viewPlayer (getPlayerXYpx m)
                , viewPortal (getPortalXYpx m)
                , viewMonsters m.clock m.monsters |> Svg.Keyed.node "g" []
                ]
             , S.lazy viewGameOver m.game
             ]
            )


viewDebug : DebugModel -> View
viewDebug { pressedKeys, game } =
    divClass "flex hs2" [ text pressedKeys, text game ]


bkgRect =
    S.rect
        [ S.width "200%"
        , S.height "200%"
        , TA.strokeWidth (px 0.2)
        , TA.stroke Color.black
        , Color.blue
            |> Light.map (\h -> { h | s = 0.7, l = 0.7 })
            |> fillColor
        ]
        []


viewGameOver game =
    let
        ( mw, mh ) =
            canvasSizeF2

        h =
            mh / 4

        y =
            (mh - h) / 2

        rectAttrs =
            [ TP.width mw
            , TP.height h
            , fillColor Color.white
            , TP.y y
            , S.opacity "0.85"
            ]

        textAttrs =
            [ fillColor Color.black
            , strokeColor Color.black
            , iFontSize 24

            --                    , FontWeightLighter |> TA.fontWeight
            , SA.strokeWidth "0.01"
            , TA.textAnchor AnchorMiddle
            , TA.alignmentBaseline AlignmentMiddle
            , S.x "50%"
            , S.y "50%"
            ]

        renderGameOver =
            S.g []
                [ S.rect rectAttrs []
                , S.text_ textAttrs [ S.text "A-Maze-Zing! You Reached Level 1" ]
                ]

        renderLevelComplete =
            S.g []
                [ S.rect rectAttrs []
                , S.text_ textAttrs [ S.text "LEVEL 2" ]
                ]
    in
        case game of
            Model.Over ->
                renderGameOver

            Model.LevelComplete ->
                renderLevelComplete

            _ ->
                S.g [] []


viewMazeWalls : Maze -> View
viewMazeWalls maze =
    Maze.concatMapCells (viewWall maze) maze |> List.concat |> S.g []


viewWall maze cord =
    let
        ( x, y ) =
            cord |> PairA.toFloat |> PairA.mul cellSize

        eastWall =
            Svg.rect
                [ TP.x (x + cellSize - (wallThickness / 2))
                , TP.y y
                , TP.width wallThickness
                , TP.height cellSize
                , SA.fill "#000"
                ]
                []

        southWall =
            Svg.rect
                [ TP.x x
                , TP.y (y + cellSize - (wallThickness / 2))
                , TP.width cellSize
                , TP.height wallThickness
                , SA.fill "#000"
                ]
                []
    in
        R.ter (Maze.isEastConnected cord maze) [] [ eastWall ]
            ++ R.ter (Maze.isSouthConnected cord maze) [] [ southWall ]


viewPlayer =
    uncurry (S.lazy2 viewPlayerHelp)


viewPortal =
    uncurry (S.lazy2 (viewEntity Color.darkPurple))


viewPlayerHelp x y =
    S.circle
        [ TP.cx (x + centerOffset)
        , TP.cy (y + centerOffset)
        , TP.r defaultRadius
        , Color.green |> Light.map (\h -> { h | s = 1, l = 0.89 }) |> fillColor
        ]
        []


viewEntity color x y =
    S.circle
        [ TP.cx (x + centerOffset)
        , TP.cy (y + centerOffset)
        , TP.r defaultRadius
        , color |> fillColor
        ]
        []


viewMonsters clock =
    List.indexedMap (\idx mon -> ( String.fromInt idx, mon |> getMonsterXYpx clock >> viewMonster ))


viewMonster ( x, y ) =
    S.lazy2 viewMonsterHelp x y


centerOffset =
    cellSize / 2


viewMonsterHelp x y =
    S.circle
        [ Color.darkOrange |> fillColor
        , TP.r defaultRadius
        , TP.cx (x + centerOffset)
        , TP.cy (y + centerOffset)
        ]
        []



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
