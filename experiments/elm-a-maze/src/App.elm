port module App exposing (..)

import Animation as A exposing (Animation)
import Browser.Dom
import Browser.Dom as BD
import Browser.Dom as B
import Color exposing (Color)
import Coordinate2D as C2
import Html as H exposing (Html)
import Html.Lazy as H
import Html.Attributes as H
import Html.Attributes as HA
import Keyboard
import Keyboard.Arrows
import Light
import List.Extra
import Ramda as R
import Random
import Size
import Svg
import Svg as S
import Svg.Attributes as S
import Svg.Attributes as SA
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
import TypedSvg.Types exposing (Fill(..), Transform(..), px)
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
        , iWidth
        , iX
        , iX1
        , iX2
        , iY
        , iY1
        , iY2
        )


---- PORTS ----


port onWindowBlur : (() -> msg) -> Sub msg



---- MODEL ----


type alias IntPair =
    ( Int, Int )


addBothPairs ( a1, b1 ) ( a2, b2 ) =
    ( a1 + a2, b1 + b2 )


multiplyBothPairs ( a1, b1 ) ( a2, b2 ) =
    ( a1 * a2, b1 * b2 )


type alias Model =
    { gridSize : IntPair
    , pxAnim : Animation
    , pyAnim : Animation
    , pressedKeys : List Keyboard.Key
    , pageLoadedAt : Int
    , clock : A.Clock
    , mazeG : MazeGenerator
    }


type alias Flags =
    { now : Int }


createAnim from to { clock } =
    A.animation clock
        |> A.from from
        |> A.to to
        |> A.ease identity
        |> A.speed 0.003


defaultAnim =
    createAnim 1 1 { clock = 0 }


gridSize =
    ( 18, 12 )


cellSize =
    32


init : Flags -> ( Model, Cmd Msg )
init { now } =
    let
        initialSeed =
            Random.initialSeed now

        ( mazeSeed, modelSeed ) =
            Random.step Random.independentSeed initialSeed
    in
        { gridSize = ( 10, 5 )
        , pxAnim = defaultAnim
        , pyAnim = defaultAnim
        , pressedKeys = []
        , pageLoadedAt = now
        , clock = 0
        , mazeG = MG.init mazeSeed gridSize |> MG.solve
        }
            |> noCmd


notRunning anim m =
    isScheduled anim m || isDone anim m


isRunning anim m =
    A.isRunning m.clock anim


isScheduled anim m =
    A.isScheduled m.clock anim


isDone anim m =
    A.isDone m.clock anim


animCurrent anim m =
    A.animate m.clock anim


animRetargetTo to anim m =
    A.retarget m.clock to anim


animToGridCellPx clock anim =
    (A.animate clock anim) * cellSize


getPlayerCellXY : Model -> ( Float, Float )
getPlayerCellXY m =
    ( m.pxAnim, m.pyAnim )
        |> R.mapBothWith (animToGridCellPx m.clock)


clampGridX x m =
    let
        ( w, _ ) =
            m.gridSize
    in
        clamp 0 (w - 1) x


clampGridY y m =
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



---- UPDATE ----


type Msg
    = NoOp
    | OnWindowBlur ()
    | KeyMsg Keyboard.Msg
    | AnimationFrameDelta Float
    | AnimationFrame Time.Posix
    | Player


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

        OnWindowBlur _ ->
            { m | pressedKeys = [] } |> noCmd

        KeyMsg keyMsg ->
            let
                ( updatedPressedKeys, keyChange ) =
                    Keyboard.updateWithKeyChange Keyboard.anyKey keyMsg m.pressedKeys
            in
                noCmd { m | pressedKeys = updatedPressedKeys }

        AnimationFrameDelta elapsed ->
            { m | gridSize = gridSize } |> noCmd

        Player ->
            let
                ( newPxAnim, newPyAnim ) =
                    getFirstArrowKey m
                        |> Maybe.map
                            (\key ->
                                let
                                    connections : Set MG.Connection
                                    connections =
                                        MG.mapConnections C2.normalizeConnection m.mazeG
                                            |> Set.fromList

                                    isConnected cp =
                                        connections |> Set.member (C2.normalizeConnection cp)

                                    ( dx, dy ) =
                                        getArrows m

                                    ( xCells, yCells ) =
                                        gridSize

                                    newPxAnim_ =
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

                                    newPyAnim_ =
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
                                        A.equals newPyAnim_ m.pyAnim |> not

                                    xChanged =
                                        A.equals newPxAnim_ m.pxAnim |> not
                                in
                                    if xChanged && (isXArrowKey key || not yChanged) then
                                        ( newPxAnim_, m.pyAnim )
                                    else if yChanged && (isYArrowKey key || not xChanged) then
                                        ( m.pxAnim, newPyAnim_ )
                                    else
                                        ( m.pxAnim, m.pyAnim )
                            )
                        |> Maybe.withDefault ( m.pxAnim, m.pyAnim )
            in
                noCmd { m | pxAnim = newPxAnim, pyAnim = newPyAnim }

        AnimationFrame posix ->
            update Player { m | clock = getClock posix m }


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

        newTo =
            to
                + newDirection
                |> clamp 0 (cellCount - 1)
    in
        if (notRunning anim m || directionReversed) && isConnected ( to, newTo ) && to /= newTo then
            let
                _ =
                    {- Debug.log "currentDirection, newDirection" ( currentDirection, newDirection ) -}
                    1
            in
                createAnim current newTo m
            --                                    |> Debug.log "pxAnim"
        else
            anim



---- VIEW ----


type alias View =
    Html Msg


worldSizeIntT =
    gridSize |> R.mapBothWith ((+) 2 >> (*) cellSize >> round)


concat a b =
    a ++ b


canvasWHStyles =
    worldSizeIntT
        |> R.mapBothWith String.fromInt
        |> R.mapBothWith (R.flip concat "px")
        |> Tuple.mapBoth (H.style "min-width") (H.style "min-height")
        |> R.tupleToList


view : Model -> View
view m =
    H.div ([ H.class "flex flex-column items-center pa2 h-100 " ] ++ canvasWHStyles)
        [ H.div [ H.class "flex flex-column vs3" ]
            [ H.div [ H.class "f3 tc" ] [ H.text "A-Maze-Zing!" ]
            , H.div
                [ H.class "flex-auto overflow-scroll"

                {- , H.style "transform" "scale( 0.8 , 0.8 )" -}
                ]
                [ viewSvg m ]
            , H.div [ H.class "" ]
                [ getDebugState m |> debugView
                ]
            ]
        ]


debugView : DebugModel -> View
debugView { pressedKeys } =
    H.div [] [ H.text pressedKeys ]


viewSvg : Model -> View
viewSvg m =
    S.svg ([{- H.style "transform" "scale( 0.9 , 0.9 )" -}] ++ canvasWHStyles)
        [ S.rect
            [ S.width "100%"
            , S.height "100%"
            , TP.x 0
            , TP.y 0
            , TA.strokeWidth (px 0.2)
            , TA.stroke Color.black
            , Color.blue
                |> Light.map (\h -> { h | s = 1, l = 0.7 })
                |> fillColor
            ]
            []
        , viewGameContent m
        ]


viewGameContent m =
    S.g [ TA.transform [ Translate cellSize cellSize ] ]
        (viewGridCells m.gridSize ++ viewMazeWalls m.mazeG ++ viewPlayer (getPlayerCellXY m))


wallThickness =
    cellSize // 10


wallThicknessF =
    toFloat wallThickness


viewMazeWalls mg =
    let
        cellSizePx =
            cellSize

        connections : Set MG.Connection
        connections =
            MG.mapConnections C2.normalizeConnection mg
                |> Set.fromList

        isConnected cp =
            Set.member cp connections

        isSouthConnected ( x, y ) =
            isConnected ( ( x, y ), ( x, y + 1 ) )

        isEastConnected ( x, y ) =
            isConnected ( ( x, y ), ( x + 1, y ) )

        size =
            cellSizePx

        viewCell cord _ =
            let
                ( x, y ) =
                    C2.scale size cord
            in
                Svg.g []
                    [ Svg.rect
                        [ iX (x + size - wallThickness)
                        , iY y
                        , iWidth wallThickness
                        , iHeight size
                        , SA.fill "#000"
                        , R.ter (isEastConnected cord) "0" "1" |> SA.opacity
                        ]
                        []
                    , Svg.rect
                        [ iX x
                        , iY (y + size - wallThickness)
                        , iWidth size
                        , iHeight wallThickness
                        , SA.fill "#000"
                        , R.ter (isSouthConnected cord) "0" "1" |> SA.opacity
                        ]
                        []
                    ]
    in
        MG.concatMapCellInfo viewCell mg


viewPlayer cord =
    let
        offset =
            5

        radius =
            (cellSize - wallThicknessF - offset) / 2

        cXYAttrs =
            cord
                |> R.mapBothWith ((+) ((cellSize - wallThicknessF) / 2))
                |> Tuple.mapBoth TP.cx TP.cy
                |> R.tupleToList

        rAttr =
            TP.r radius
    in
        [ S.circle (cXYAttrs ++ [ rAttr, fillColor Color.lightOrange ]) []
        ]


viewGridCells size =
    gridConcatMap size viewGridCell


cordToPx =
    R.mapBothWith (toFloat >> (*) cellSize)


viewGridCell cord =
    let
        xyAttr =
            cord |> cordToPx |> R.mapBothWith ((+) (-wallThicknessF / 2)) |> Tuple.mapBoth TP.x TP.y |> R.tupleToList

        whAttr =
            px cellSize |> \s -> ( s, s ) |> Tuple.mapBoth TA.width TA.height |> R.tupleToList
    in
        S.rect (xyAttr ++ whAttr ++ [ strokeColor Color.black, TP.strokeWidth 1, TA.noFill, opacityFloat 0.05 ]) []


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
        [ B.onAnimationFrameDelta AnimationFrameDelta
        , B.onAnimationFrame AnimationFrame
        , Sub.map KeyMsg Keyboard.subscriptions
        , onWindowBlur OnWindowBlur
        ]


main : Program Flags Model Msg
main =
    B.element
        { view = H.lazy view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
