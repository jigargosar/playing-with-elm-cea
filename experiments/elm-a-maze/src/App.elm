module App exposing (..)

import Color exposing (Color)
import Html as H exposing (Html)
import Html.Lazy as H
import Html.Attributes as H
import Html.Attributes as HA
import Keyboard
import Keyboard.Arrows
import Light
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


addIntPair ( a1, b1 ) ( a2, b2 ) =
    ( a1 + a2, b1 + b2 )


type alias Model =
    { keySet : Set String
    , gridSize : IntPair
    , playerPos : IntPair
    , playerVelocity : IntPair
    , pressedKeys : List Keyboard.Key
    , posUpdatedAt : Int
    }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    { keySet = Set.empty
    , gridSize = ( 10, 5 )
    , playerPos = ( 0, 0 )
    , playerVelocity = ( 0, 0 )
    , pressedKeys = []
    , posUpdatedAt = now
    }
        |> noCmd


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



---- UPDATE ----


type Msg
    = NoOp
    | KeyMsg Keyboard.Msg
    | AnimationFrameDelta Float
    | AnimationFrame Time.Posix
    | KeyDown String
    | KeyUp String


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
                noCmd { m | pressedKeys = updatedPressedKeys, playerVelocity = arrowsToDirectionVec m }

        AnimationFrameDelta elapsed ->
            noCmd m

        AnimationFrame posix ->
            let
                now =
                    Time.posixToMillis posix

                canUpdatePos =
                    (now - m.posUpdatedAt) > (300)

                newPos =
                    computeNewPos m

                newModel =
                    (if canUpdatePos && not (newPos == m.playerPos) then
                        { m | playerPos = newPos, posUpdatedAt = now, playerVelocity = ( 0, 0 ) }
                     else
                        m
                    )
            in
                noCmd newModel

        KeyDown key ->
            { m | keySet = Set.insert key m.keySet } |> noCmd

        KeyUp key ->
            { m | keySet = Set.remove key m.keySet } |> noCmd


arrowsToDirectionVec m =
    Keyboard.Arrows.arrows m.pressedKeys
        |> (\{ x, y } -> ( x, -y ))


computeNewPos m =
    m.playerVelocity
        |> addIntPair m.playerPos
        |> Tuple.mapBoth (\x -> clampGridX x m) (\y -> clampGridY y m)



---- VIEW ----


type alias View =
    Html Msg


worldSize =
    Size.fromComponent ( 600, 350 )


worldSizeIntT =
    Size.toRoundIntComponent worldSize


view : Model -> View
view m =
    H.div [ HA.class "flex flex-column items-center pa2 h-100 " ]
        [ H.div [ HA.class "flex flex-column vs3" ]
            [ H.div [ HA.class "f3" ] [ H.text "SVG API" ]
            , viewSvg m
            ]
        ]


viewSvg : Model -> View
viewSvg m =
    let
        attrs =
            worldSize
                |> Size.toComponent
                |> Tuple.mapBoth TP.width TP.height
                |> R.tupleToList
    in
        S.svg attrs
            [ S.rect
                [ S.width "100%"
                , S.height "100%"
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
        (viewGridCells m.gridSize ++ viewPlayer m.playerPos)


viewPlayer cord =
    let
        offset =
            5.0

        xyAttrs =
            cord
                |> R.mapBothWith (toFloat >> (*) cellSize >> (+) offset)
                |> Tuple.mapBoth TP.x TP.y
                |> R.tupleToList

        whAttrs =
            cellSize
                |> (+) (-offset * 2)
                |> px
                |> R.toTuple
                |> Tuple.mapBoth TA.width TA.height
                |> R.tupleToList

        cXYAttrs =
            cord
                |> R.mapBothWith (toFloat >> (*) cellSize >> (+) (cellSize / 2.0))
                |> Tuple.mapBoth TP.cx TP.cy
                |> R.tupleToList

        rAttr =
            (cellSize - offset) / 2 |> TP.r
    in
        [ S.rect (xyAttrs ++ whAttrs ++ [ fillColor Color.lightOrange, opacityFloat 0 ]) []
        , S.circle (cXYAttrs ++ [ rAttr, fillColor Color.lightOrange ]) []
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
        , B.onKeyDown (D.map KeyDown (D.field "key" D.string))
        , B.onKeyUp (D.map KeyUp (D.field "key" D.string))
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
