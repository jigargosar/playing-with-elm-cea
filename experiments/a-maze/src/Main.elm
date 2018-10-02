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
    | Solve
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

        Solve ->
            let
                ( newMazeGen, newSeed ) =
                    MazeGenerator.solve m.seed m.mazeGenerator
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
                    [ onClick Solve
                    , disabled (MazeGenerator.isSolved m.mazeGenerator)
                    ]
                    [ text "Solve" ]
                , button
                    [ onClick RemoveRandomConnections
                    ]
                    [ text "RemoveRandomConnections" ]
                ]
            , div [ class "no-sel" ] [ viewSvg m ]
            ]
        ]


viewSvg m =
    let
        canvasWidth =
            650

        canvasHeight =
            500
    in
    svg
        [ SA.class "flex center"
        , iWidth (canvasWidth + 10)
        , iHeight (canvasHeight + 10)
        ]
        (viewMazeGenerator m.mazeGenerator |> ViewSvgHelpers.view)


cellSizePx =
    50


innerOffsetPx =
    cellSizePx // 5


gridSquare : Coordinate2D -> MazeGenerator -> Svg msg
gridSquare cord m =
    let
        ( x, y ) =
            cord

        isVisited =
            MazeGenerator.isVisitedCord cord m

        isOnTopOfStack =
            MazeGenerator.getIsOnTopOfStack cord m

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


viewMazeGenerator : MazeGenerator -> List (Svg msg)
viewMazeGenerator m =
    let
        { width, height } =
            MazeGenerator.getDimensions m

        transform =
            Coordinate2D.scale cellSizePx
                >> Coordinate2D.translate (cellSizePx // 2)

        viewCellConnections =
            let
                lines : List (Svg msg)
                lines =
                    MazeGenerator.getConnections m
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
    viewCellConnections ++ Coordinate2D.flatMap width height (flip gridSquare m)



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
