module Main exposing (main)

import AMaze exposing (AMaze)
import Array2D
import Browser
import Browser.Events
import Coordinate2D exposing (Coordinate2D)
import Data2D
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick, onDoubleClick)
import ISvg exposing (iCX, iCY, iHeight, iTranslate, iWidth, iX, iY)
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


type alias CellData =
    { isVisited : Bool }


defaultCellData : CellData
defaultCellData =
    { isVisited = False }


type alias Lookup =
    Dict Coordinate2D CellData


emptyLookup : Lookup
emptyLookup =
    Dict.empty


initialLookup : Lookup
initialLookup =
    emptyLookup |> Dict.insert ( 0, 0 ) { isVisited = True }


type alias Model =
    { seed : Random.Seed, maze : AMaze, lookup : Lookup }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    update WalledAMaze
        { seed = Random.initialSeed now
        , maze = walledMaze
        , lookup = initialLookup
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


getCellData : Coordinate2D -> Model -> CellData
getCellData cord m =
    m.lookup |> Dict.get cord |> Maybe.withDefault defaultCellData



---- UPDATE ----


type Msg
    = NoOp
    | RandomAMaze
    | WalledAMaze
    | Step


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        RandomAMaze ->
            updateGenerateNewMaze m |> pure

        WalledAMaze ->
            { m | maze = AMaze.fillWalls m.maze } |> pure

        Step ->
            let
                _ =
                    1
            in
            pure m


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

                {- , button [ onClick RandomAMaze ] [ text "Random" ]
                   , button [ onClick WalledAMaze ] [ text "Walled" ]
                -}
                , button [ onClick Step ] [ text "Step" ]
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
    100


innerOffsetPx =
    20


gridSquare m cord =
    let
        ( x, y ) =
            cord

        isVisited =
            getCellData cord m |> .isVisited

        sizeWithOffset =
            cellSizePx + (innerOffsetPx * 2)

        xyMultiplier =
            cellSizePx

        size =
            cellSizePx - (innerOffsetPx * 2)
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
            [ iX innerOffsetPx
            , iY innerOffsetPx
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
        , Svg.circle
            [ cellSizePx // 2 |> iCX
            , cellSizePx // 2 |> iCY
            , SA.r "5"
            , ter isVisited "blue" "none" |> SA.fill
            ]
            []
        ]


viewAlgoData m =
    {- viewAMaze m -}
    let
        drawMazeCell cord =
            gridSquare m cord
    in
    Coordinate2D.flatMap 6 4 drawMazeCell
        |> Svg.g []


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
