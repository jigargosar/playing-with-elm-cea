module Main exposing (main)

import AMaze exposing (AMaze)
import Array2D
import Browser
import Browser.Events
import Coordinate2D exposing (Coordinate2D)
import Data2D
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, img, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick, onDoubleClick)
import ISvg exposing (iCX, iCY, iHeight, iTranslate, iWidth, iX, iX1, iX2, iY, iY1, iY2)
import Ramda exposing (equals, flip, ifElse, isEmptyList, ter)
import Random
import Random.Array
import Random.Extra
import Random.List
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


type alias CStack =
    List Coordinate2D


initialCStack =
    [ ( 0, 0 ) ]


type alias Model =
    { seed : Random.Seed, maze : AMaze, lookup : Lookup, cStack : CStack }


type alias Flags =
    { now : Int }


init : Flags -> ( Model, Cmd Msg )
init { now } =
    update WalledAMaze
        { seed = Random.initialSeed now
        , maze = walledMaze
        , lookup = initialLookup
        , cStack = initialCStack
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


getCellData : Coordinate2D -> Model -> CellData
getCellData cord m =
    m.lookup |> Dict.get cord |> Maybe.withDefault defaultCellData


getCellDataIn : Model -> Coordinate2D -> CellData
getCellDataIn =
    flip getCellData


getIsCellVisited : Coordinate2D -> Model -> Bool
getIsCellVisited cord =
    getCellData cord >> .isVisited


getIsCellVisitedIn : Model -> Coordinate2D -> Bool
getIsCellVisitedIn =
    flip getIsCellVisited


getVisitedCellCount : Model -> Int
getVisitedCellCount m =
    Coordinate2D.flatMap hCellCount vCellCount (getCellDataIn m)
        |> List.filter .isVisited
        |> List.length


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


getStackTopCellData m =
    m.cStack
        |> List.head
        |> Maybe.map (getCellDataIn m)
        |> Maybe.withDefault defaultCellData



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
            m |> ifElse getIsSolved identity updateStep |> pure


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
        cordCD =
            getCellData cord model

        newLookup =
            model.lookup
                |> Dict.insert cord { cordCD | isVisited = True }
    in
    { model | lookup = newLookup, cStack = cord :: model.cStack }


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
            getCellData cord m |> .isVisited

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
    {- viewAMaze m -}
    let
        drawMazeCell cord =
            gridSquare m cord

        viewCells =
            Coordinate2D.flatMap hCellCount vCellCount drawMazeCell
                |> Svg.g []

        viewCellConnections =
            let
                transform =
                    Coordinate2D.scale cellSizePx
                        >> Coordinate2D.translate (cellSizePx // 2)

                ( x1, y1 ) =
                    ( 1, 1 ) |> transform

                ( x2, y2 ) =
                    ( 2, 1 ) |> transform
            in
            Svg.g []
                [ Svg.line
                    [ iX1 x1
                    , iX2 x2
                    , iY1 y1
                    , iY2 y2
                    , SA.strokeWidth "2"
                    , SA.stroke "#000"
                    ]
                    []
                ]
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
