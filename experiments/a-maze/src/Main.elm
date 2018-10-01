module Main exposing (main)

import AMaze exposing (AMaze)
import Array2D
import Browser
import Browser.Events
import Coordinate2D exposing (Coordinate2D)
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick, onDoubleClick)
import ISvg exposing (iCX, iCY, iHeight, iTranslate, iWidth, iX, iX1, iX2, iY, iY1, iY2)
import MazeGenerator exposing (MazeGenerator)
import Ramda exposing (equals, flip, ifElse, isEmptyList, ter)
import Random
import Random.Array
import Random.Extra
import Random.List
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes as SA
import TypedSvg exposing (svg)
import TypedSvg.Attributes exposing (alignmentBaseline)
import TypedSvg.Types exposing (AlignmentBaseline(..))
import ViewAMaze
import ViewSvgHelpers



---- MODEL ----


type alias Connection =
    ( Coordinate2D, Coordinate2D )


type alias Connections =
    Set Connection


initialConnections : Connections
initialConnections =
    Set.empty


type alias VisitedCords =
    Set Coordinate2D


emptyVisitedCords : VisitedCords
emptyVisitedCords =
    Set.empty


initialVisitedCords : VisitedCords
initialVisitedCords =
    Set.fromList [ ( 0, 0 ) ]


type alias CStack =
    List Coordinate2D


initialCStack =
    [ ( 0, 0 ) ]


type alias Model =
    { seed : Random.Seed
    , maze : AMaze
    , visitedCords : VisitedCords
    , cStack : CStack
    , connections : Connections
    , mazeGenerator : MazeGenerator
    }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    update WalledAMaze
        { seed = Random.initialSeed now
        , maze = walledMaze
        , visitedCords = initialVisitedCords
        , cStack = initialCStack
        , connections = initialConnections
        , mazeGenerator = MazeGenerator.init 6 4
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


hCellCount =
    6


vCellCount =
    4


totalCellCount =
    hCellCount * vCellCount


getIsSolved : Model -> Bool
getIsSolved =
    getVisitedCellCount >> equals totalCellCount


getIsCellVisited : Coordinate2D -> Model -> Bool
getIsCellVisited cord =
    .visitedCords >> Set.member cord


getIsCellVisitedIn : Model -> Coordinate2D -> Bool
getIsCellVisitedIn =
    flip getIsCellVisited


getVisitedCellCount : Model -> Int
getVisitedCellCount m =
    m.visitedCords |> Set.size


getIsOnTopOfStack : Coordinate2D -> Model -> Bool
getIsOnTopOfStack cord m =
    m.cStack |> List.head |> Maybe.map (equals cord) |> Maybe.withDefault False


isValidCord : Coordinate2D -> Bool
isValidCord ( x, y ) =
    x >= 0 && y >= 0 && x < hCellCount && y < vCellCount


getValidNeighbourCords : Coordinate2D -> List Coordinate2D
getValidNeighbourCords =
    Coordinate2D.perpendicularNeighboursOf >> List.filter isValidCord


getUnVisitedNeighboursOfTopOfStack m =
    m.cStack
        |> List.head
        |> Maybe.map (getValidNeighbourCords >> List.filter (getIsCellVisitedIn m >> not))
        |> Maybe.withDefault []



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
            m |> ifElse getIsSolved identity updateStep |> updateMazeGeneratorStep |> pure


updateMazeGeneratorStep : Model -> Model
updateMazeGeneratorStep m =
    let
        ( newMazeGen, newSeed ) =
            MazeGenerator.step m.seed m.mazeGenerator
    in
    { m | mazeGenerator = newMazeGen, seed = newSeed }


updateStep m =
    let
        _ =
            ( getVisitedCellCount m, totalCellCount ) |> Debug.log "(visited,total)"

        neighbourCordGenerator =
            getUnVisitedNeighboursOfTopOfStack m
                |> Debug.log "getValidNeighbourCords"
                |> Random.List.choose

        ( ( maybeCord, _ ), newSeed ) =
            Random.step neighbourCordGenerator m.seed

        _ =
            maybeCord |> Debug.log "randomCord"

        newModel =
            { m | seed = newSeed }
    in
    case maybeCord of
        Just cord ->
            updateVisitCell cord newModel

        Nothing ->
            updatePopStack newModel


updatePopStack model =
    { model | cStack = model.cStack |> List.drop 1 }


updateVisitCell cord model =
    let
        newVisitedCords =
            model.visitedCords
                |> Set.insert cord

        newConnections =
            model.cStack
                |> List.head
                |> Maybe.map
                    (\stackTop ->
                        model.connections
                            |> Set.insert ( stackTop, cord )
                    )
                |> Maybe.withDefault model.connections
    in
    { model
        | visitedCords = newVisitedCords
        , cStack = cord :: model.cStack
        , connections = newConnections
    }


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
                , button [ onClick Step, disabled (getIsSolved m) ] [ text "Step" ]
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
            m.visitedCords |> Set.member cord

        isOnTopOfStack =
            getIsOnTopOfStack cord m

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
        , Svg.circle
            [ cellSizePx // 2 |> iCX
            , cellSizePx // 2 |> iCY
            , SA.r "5"
            , ter isOnTopOfStack "red" "none" |> SA.fill
            ]
            []
        ]


viewAlgoData m =
    let
        drawMazeCell cord =
            gridSquare m cord

        viewCells =
            Coordinate2D.flatMap hCellCount vCellCount drawMazeCell
                |> Svg.g []

        transform =
            Coordinate2D.scale cellSizePx
                >> Coordinate2D.translate (cellSizePx // 2)

        viewCellConnections =
            let
                lines : List (Svg msg)
                lines =
                    m.connections
                        |> Set.toList
                        |> List.map viewCellConnection
            in
            Svg.g [] lines

        viewCellConnection ( from, to ) =
            let
                ( x1, y1 ) =
                    from |> transform

                ( x2, y2 ) =
                    to |> transform
            in
            Svg.line
                [ iX1 x1
                , iX2 x2
                , iY1 y1
                , iY2 y2
                , SA.strokeWidth "3"
                , SA.stroke "blue"
                , SA.opacity "0.5"
                ]
                []
    in
    Svg.g [] [ viewCells, viewCellConnections ]


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
