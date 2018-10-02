module Main exposing (main)

import Array2D
import Browser
import Browser.Events
import Coordinate2D exposing (Coordinate2D)
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick, onDoubleClick)
import ISvg exposing (iCX, iCY, iFontSize, iHeight, iTranslate, iWidth, iX, iX1, iX2, iY, iY1, iY2)
import MazeGenerator exposing (MazeGenerator)
import Ramda exposing (equals, flip, ifElse, isEmptyList, ter)
import Random
import Random.Array
import Random.Extra
import Random.List
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Keyed
import TypedSvg exposing (svg)
import TypedSvg.Attributes exposing (alignmentBaseline, strokeLinecap)
import TypedSvg.Types exposing (AlignmentBaseline(..), StrokeLinecap(..))
import ViewSvgHelpers



---- MODEL ----


type alias Model =
    { seed : Random.Seed
    , mazeGenerator : MazeGenerator
    }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    pure
        { seed = Random.initialSeed now
        , mazeGenerator = MazeGenerator.init 12 6
        }



---- UPDATE ----


type Msg
    = NoOp
    | Step
    | End
    | RemoveRandomConnections


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        Step ->
            let
                ( newMazeGen, newSeed ) =
                    MazeGenerator.step m.seed m.mazeGenerator
            in
            { m | mazeGenerator = newMazeGen, seed = newSeed } |> pure

        End ->
            let
                ( newMazeGen, newSeed ) =
                    MazeGenerator.step m.seed m.mazeGenerator
            in
            { m | mazeGenerator = newMazeGen, seed = newSeed } |> pure

        RemoveRandomConnections ->
            let
                ( newMazeGenerator, newSeed ) =
                    MazeGenerator.removeRandomConnections m.seed m.mazeGenerator
            in
            pure { m | mazeGenerator = newMazeGenerator, seed = newSeed }


pure model =
    ( model, Cmd.none )



---- VIEW ----


worldWidth =
    650


worldHeight =
    500


view : Model -> Html Msg
view m =
    div []
        [ div [ class "pa3 vs3" ]
            [ div [ class "flex items-end hs3" ]
                [ div [ class "f2" ] [ text "A-Maze" ]
                , button
                    [ onClick Step
                    , disabled (MazeGenerator.isSolved m.mazeGenerator)
                    ]
                    [ text "Step" ]
                , button
                    [ onClick RemoveRandomConnections
                    ]
                    [ text "RemoveRandomConnections" ]
                ]
            , div [ class "no-sel" ]
                [ svg
                    [ SA.class "flex center"
                    , iWidth (worldWidth + 10)
                    , iHeight (worldHeight + 10)
                    ]
                    (viewAlgoData m |> ViewSvgHelpers.view)
                ]
            ]
        ]


cellSizePx =
    50


innerOffsetPx =
    cellSizePx // 5


gridSquare : Model -> Coordinate2D -> Svg msg
gridSquare m cord =
    let
        ( x, y ) =
            cord

        isVisited =
            m.mazeGenerator |> MazeGenerator.isVisitedCord cord

        isOnTopOfStack =
            MazeGenerator.getIsOnTopOfStack cord m.mazeGenerator

        sizeWithOffset =
            cellSizePx + (innerOffsetPx * 2)

        size =
            cellSizePx - (innerOffsetPx * 2)
    in
    Svg.g
        [ SA.transform (iTranslate (x * cellSizePx) (y * cellSizePx)) ]
        [ Svg.rect
            [ iX innerOffsetPx
            , iY innerOffsetPx
            , iWidth size
            , iHeight size
            , SA.fill "#cd37a9"
            , SA.fill "none"
            , SA.strokeWidth "3"
            , SA.stroke "#000"
            , SA.opacity "0.1"
            ]
            []
        , Svg.text_ [ iFontSize innerOffsetPx, alignmentBaseline AlignmentTextBeforeEdge ]
            [ [ "("
              , String.fromInt x
              , ","
              , String.fromInt y
              , ")"
              ]
                |> String.join ""
                |> Svg.text
            ]
        , Svg.g [ SA.opacity "0.5" ]
            [ Svg.circle
                [ cellSizePx // 2 |> iCX
                , cellSizePx // 2 |> iCY
                , SA.r "5"
                , ter isVisited "blue" "none" |> SA.fill
                ]
                []
            , Svg.circle
                [ cellSizePx // 2 |> iCX
                , cellSizePx // 2 |> iCY
                , SA.r "5"
                , ter isOnTopOfStack "red" "none" |> SA.fill
                ]
                []
            ]
        ]


viewAlgoData : Model -> List (Svg msg)
viewAlgoData m =
    let
        drawMazeCell cord =
            gridSquare m cord

        { width, height } =
            MazeGenerator.getDimensions m.mazeGenerator

        viewCells =
            Coordinate2D.flatMap width height drawMazeCell

        transform =
            Coordinate2D.scale cellSizePx
                >> Coordinate2D.translate (cellSizePx // 2)

        viewCellConnections =
            let
                lines : List (Svg msg)
                lines =
                    MazeGenerator.getConnections m.mazeGenerator
                        |> Set.toList
                        |> List.map viewCellConnection
            in
            lines

        viewCellConnection ( from, to ) =
            let
                ( x1, y1 ) =
                    from |> transform

                ( x2, y2 ) =
                    to |> transform
            in
            Svg.line
                [ iX1 x1
                , iY1 y1
                , iX2 x2
                , iY2 y2
                , SA.strokeWidth "3"
                , SA.stroke "blue"
                , SA.opacity "0.7"
                , strokeLinecap StrokeLinecapRound
                ]
                []
    in
    viewCellConnections ++ viewCells



---- PROGRAM ----


subscriptions _ =
    Sub.batch [ Browser.Events.onAnimationFrameDelta (\_ -> NoOp) ]


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
