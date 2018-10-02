module MazeGenerator exposing
    ( CellInfo
    , Connection
    , MazeGenerator
    , concatMap
    , init
    , isSolved
    , mapConnections
    , removeRandomConnections
    , solve
    , step
    )

import Connection2D exposing (Connection2D)
import Coordinate2D exposing (Coordinate2D)
import Ramda exposing (ensureAtLeast, equals)
import Random
import Random.Array
import Random.List
import Random.Set
import Set exposing (Set)


type alias Connection =
    Connection2D


type alias ConnectionSet =
    Set Connection


type alias VisitedSet =
    Set Coordinate2D


type alias Stack =
    List Coordinate2D


type alias Record =
    { visitedSet : VisitedSet
    , stack : Stack
    , connectionSet : ConnectionSet
    , width : Int
    , height : Int
    , seed : Random.Seed
    }


type MazeGenerator
    = MazeGenerator Record


type alias MazeGeneratorF =
    MazeGenerator -> MazeGenerator


init : Random.Seed -> Int -> Int -> MazeGenerator
init seed width height =
    let
        rec : Record
        rec =
            { visitedSet = Set.fromList [ ( 0, 0 ) ]
            , stack = [ ( 0, 0 ) ]
            , connectionSet = Set.empty
            , width = ensureAtLeast 1 width
            , height = ensureAtLeast 1 height
            , seed = seed
            }
    in
    MazeGenerator rec


getTotalCellCount : Record -> Int
getTotalCellCount { width, height } =
    width * height


getVisitedCellCount : Record -> Int
getVisitedCellCount { visitedSet } =
    Set.size visitedSet


isSolvedRec : Record -> Bool
isSolvedRec rec =
    getVisitedCellCount rec == getTotalCellCount rec


isSolved : MazeGenerator -> Bool
isSolved (MazeGenerator rec) =
    isSolvedRec rec


getConnections : MazeGenerator -> ConnectionSet
getConnections (MazeGenerator rec) =
    rec.connectionSet


step : MazeGeneratorF
step (MazeGenerator rec) =
    (if isSolvedRec rec then
        rec

     else
        case rec.stack of
            top :: rest ->
                stepHelp top rec

            _ ->
                rec
    )
        |> MazeGenerator


solve : MazeGeneratorF
solve mg =
    if isSolved mg then
        mg

    else
        step mg |> solve


stepHelp : Coordinate2D -> Record -> Record
stepHelp stackTop rec =
    let
        isWithinBounds : Coordinate2D -> Bool
        isWithinBounds ( x, y ) =
            x >= 0 && y >= 0 && x < rec.width && y < rec.height

        isVisitedRec cord =
            Set.member cord rec.visitedSet

        unVisitedNeighbours =
            stackTop
                |> Coordinate2D.perpendicularNeighboursOf
                |> List.filter isWithinBounds
                |> List.filter (isVisitedRec >> not)

        unVisitedNeighbourGenerator =
            unVisitedNeighbours
                |> Random.List.choose

        ( ( maybeCord, _ ), newSeed ) =
            Random.step unVisitedNeighbourGenerator rec.seed
    in
    case maybeCord of
        Just cord ->
            { rec
                | seed = newSeed
                , visitedSet = rec.visitedSet |> Set.insert cord
                , stack = cord :: rec.stack
                , connectionSet =
                    rec.connectionSet
                        |> Set.insert ( stackTop, cord )
            }

        Nothing ->
            { rec
                | seed = newSeed
                , stack = rec.stack |> List.drop 1
            }


getDimensions (MazeGenerator { width, height }) =
    { width = width, height = height }


isCordOnTopOfStack : Coordinate2D -> MazeGenerator -> Bool
isCordOnTopOfStack cord (MazeGenerator { stack }) =
    stack |> List.head |> Maybe.map (equals cord) |> Maybe.withDefault False


isCordVisited cord (MazeGenerator { visitedSet }) =
    Set.member cord visitedSet


removeRandomConnections : MazeGeneratorF
removeRandomConnections (MazeGenerator rec) =
    let
        connectionSetGenerator : Random.Generator ConnectionSet
        connectionSetGenerator =
            rec.connectionSet
                |> Set.toList
                |> Random.List.shuffle
                |> Random.map (List.drop 10 >> Set.fromList)

        visitedSetGenerator : Random.Generator VisitedSet
        visitedSetGenerator =
            rec.visitedSet
                |> Set.toList
                |> Random.List.shuffle
                |> Random.map (List.drop 10 >> Set.fromList)

        newRecGenerator : Random.Generator Record
        newRecGenerator =
            Random.map2 (\c v -> { rec | connectionSet = c, visitedSet = v })
                connectionSetGenerator
                visitedSetGenerator

        ( newRec, newSeed ) =
            Random.step newRecGenerator rec.seed
    in
    MazeGenerator { newRec | seed = newSeed }


type alias CellInfo =
    { visited : Bool, current : Bool }


concatMap : (Coordinate2D -> CellInfo -> a) -> MazeGenerator -> List a
concatMap fn mg =
    let
        mapper cord =
            fn cord
                { visited = isCordVisited cord mg
                , current = isCordOnTopOfStack cord mg
                }

        { width, height } =
            getDimensions mg
    in
    Coordinate2D.concatMap width height mapper


mapConnections : (Connection -> a) -> MazeGenerator -> List a
mapConnections fn mg =
    getConnections mg
        |> Set.toList
        |> List.map fn
