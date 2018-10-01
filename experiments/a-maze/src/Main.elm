module Main exposing (main)

import AMaze exposing (AMaze)
import Array2D
import Browser
import Browser.Events
import Coordinate2D exposing (Coordinate2D)
import Data2D
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onDoubleClick)
import ISvg exposing (iHeight, iTranslate, iWidth, iX, iY)
import Ramda exposing (equals, ifElse, isEmptyList, ter)
import Random
import Random.Array
import Random.Extra
import Svg
import Svg.Attributes as SA
import TypedSvg exposing (svg)
import TypedSvg.Attributes exposing (alignmentBaseline)
import TypedSvg.Types exposing (AlignmentBaseline(..))
import ViewAMaze
import ViewSvgHelpers



---- MODEL ----


type alias Model =
    { seed : Random.Seed, maze : AMaze, algoData : {} }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    update WalledAMaze
        { seed = Random.initialSeed now
        , maze = walledMaze
        , algoData = {}
        }


mazeWidth =
    18


mazeHeight =
    15


generateRandomMaze : Random.Seed -> ( AMaze, Random.Seed )
generateRandomMaze =
    Random.step (AMaze.randomGenerator mazeWidth mazeHeight)


walledMaze : AMaze
walledMaze =
    AMaze.walled mazeWidth mazeHeight



---- UPDATE ----


type Msg
    = NoOp
    | RandomAMaze
    | WalledAMaze


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        RandomAMaze ->
            updateGenerateNewMaze m |> pure

        WalledAMaze ->
            { m | maze = AMaze.fillWalls m.maze } |> pure


pure model =
    ( model, Cmd.none )


updateGenerateNewMaze m =
    let
        ( newMaze, newSeed ) =
            generateRandomMaze m.seed
    in
    { m | seed = newSeed, maze = newMaze }



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
                , button [ onClick RandomAMaze ] [ text "Random" ]
                , button [ onClick WalledAMaze ] [ text "Walled" ]
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


isVisited cord m =
    cord == ( 0, 0 )


gridSquare x y cellSize innerOffset =
    let
        sizeWithOffset =
            cellSize + (innerOffset * 2)

        xyMultiplier =
            cellSize

        size =
            cellSize - (innerOffset * 2)
    in
    Svg.g
        [ SA.transform
            (iTranslate (x * xyMultiplier)
                (y
                    * xyMultiplier
                )
            )
        ]
        [ Svg.rect
            [ iX innerOffset
            , iY innerOffset
            , iWidth size
            , iHeight size
            , SA.fill "#cd37a9"
            , SA.fill "none"
            , SA.strokeWidth "2"
            , SA.stroke "#000"
            ]
            []
        , Svg.text_ [ SA.fontSize "10", alignmentBaseline AlignmentTextBeforeEdge ]
            [ [ "("
              , String.fromInt x
              , ","
              , String.fromInt y
              , ")"
              ]
                |> String.join ""
                |> Svg.text
            ]
        ]


viewAlgoData m =
    {- viewAMaze m -}
    let
        cellSizePx =
            100

        spacingPx =
            20

        drawMazeCell ( x, y ) =
            gridSquare x y cellSizePx spacingPx

        gridOffset =
            cellSizePx * 0
    in
    Coordinate2D.flatMap 6 4 drawMazeCell
        |> Svg.g [ SA.transform (iTranslate gridOffset gridOffset) ]


viewAMaze m =
    ViewAMaze.view m.maze



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
