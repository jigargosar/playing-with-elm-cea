module App exposing (..)

import Color exposing (Color)
import Html as H exposing (Html)
import Html.Lazy as H
import Html.Attributes as H
import Html.Attributes as HA
import Light
import Ramda as R
import Size
import Svg
import Svg as S
import Svg.Attributes as S
import Svg.Attributes as SA
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


type alias Model =
    { keySet : Set String
    , gridSize : IntPair
    , playerPos : IntPair
    }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    { keySet = Set.empty
    , gridSize = ( 10, 5 )
    , playerPos = ( 0, 0 )
    }
        |> noCmd



---- UPDATE ----


type Msg
    = NoOp
    | AnimationFrame Float
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

        AnimationFrame elapsed ->
            noCmd m

        KeyDown key ->
            { m | keySet = Set.insert key m.keySet } |> noCmd

        KeyUp key ->
            { m | keySet = Set.remove key m.keySet } |> noCmd



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
        xyAttr =
            cord |> R.mapBothWith (toFloat >> (*) cellSize >> px) |> Tuple.mapBoth TA.x TA.y |> R.tupleToList

        whAttr =
            px cellSize |> \s -> ( s, s ) |> Tuple.mapBoth TA.width TA.height |> R.tupleToList
    in
        [ S.rect (xyAttr ++ whAttr ++ [ fillColor Color.lightOrange ]) [] ]


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
        [ B.onAnimationFrameDelta AnimationFrame
        , B.onKeyDown (D.map KeyDown (D.field "key" D.string))
        , B.onKeyUp (D.map KeyUp (D.field "key" D.string))
        ]


main : Program Flags Model Msg
main =
    B.element
        { view = H.lazy view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
