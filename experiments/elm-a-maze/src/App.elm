module App exposing (..)

import Animation as A exposing (Animation)
import Color exposing (Color)
import Html as H exposing (Html)
import Html.Lazy as H
import Html.Attributes as H
import Html.Attributes as HA
import Keyboard
import Keyboard.Arrows
import Light
import List.Extra
import Ramda as R
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
import Browser as B
import Browser.Events as B
import Browser.Events as BE
import Set exposing (Set)
import Json.Decode as D
import Json.Encode as E
import TypedSvg.Types exposing (Fill(..), Transform(..), px)


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
    createAnim 0 0 { clock = 0 }


gridSize =
    ( 10, 8 )


init : Flags -> ( Model, Cmd Msg )
init { now } =
    { gridSize = ( 10, 5 )
    , pxAnim = defaultAnim
    , pyAnim = defaultAnim
    , pressedKeys = []
    , pageLoadedAt = now
    , clock = 0
    }
        |> Debug.log "initModel"
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
                computeNewAnim cellCount dd anim =
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
                        if (notRunning anim m || directionReversed) && to /= newTo then
                            let
                                _ =
                                    {- Debug.log "currentDirection, newDirection" ( currentDirection, newDirection ) -}
                                    1
                            in
                                createAnim current newTo m
                            --                                    |> Debug.log "pxAnim"
                        else
                            anim

                ( x, y ) =
                    getArrows m

                ( xCells, yCells ) =
                    gridSize

                newPxAnim =
                    if x /= 0 && notRunning m.pyAnim m then
                        computeNewAnim xCells x m.pxAnim
                    else
                        m.pxAnim

                newPyAnim =
                    if y /= 0 && notRunning newPxAnim m then
                        computeNewAnim yCells y m.pyAnim
                    else
                        m.pyAnim
            in
                noCmd { m | pxAnim = newPxAnim, pyAnim = newPyAnim }

        AnimationFrame posix ->
            update Player { m | clock = getClock posix m }



---- VIEW ----


type alias View =
    Html Msg


worldSize =
    Size.fromComponent ( 600, 500 )


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


cellSize =
    50.0


viewGameContent m =
    S.g [ TA.transform [ Translate cellSize cellSize ] ]
        (viewGridCells m.gridSize ++ viewPlayer (getPlayerCellXY m))


viewPlayer cord =
    let
        offset =
            5.0

        cXYAttrs =
            cord
                |> R.mapBothWith ((+) (cellSize / 2.0))
                |> Tuple.mapBoth TP.cx TP.cy
                |> R.tupleToList

        rAttr =
            (cellSize - offset) / 2 |> TP.r
    in
        [ S.circle (cXYAttrs ++ [ rAttr, fillColor Color.lightOrange ]) []
        ]


viewGridCells size =
    gridConcatMap size viewGridCell


viewGridCell cord =
    let
        xyAttr =
            cord |> R.mapBothWith (toFloat >> (*) cellSize >> px) |> Tuple.mapBoth TA.x TA.y |> R.tupleToList

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
        [ B.onAnimationFrameDelta AnimationFrameDelta
        , B.onAnimationFrame AnimationFrame
        , Sub.map KeyMsg Keyboard.subscriptions
        ]


main : Program Flags Model Msg
main =
    B.element
        { view = H.lazy view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
