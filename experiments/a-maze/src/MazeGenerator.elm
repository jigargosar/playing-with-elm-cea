module MazeGenerator exposing
    ( MazeGenerator
    , getConnections
    , getDimensions
    , getIsOnTopOfStack
    , init
    , isSolved
    , isVisitedCord
    , removeRandomConnections
    , step
    )

import Coordinate2D exposing (Coordinate2D)
import Ramda exposing (equals)
import Random
import Random.Array
import Random.List
import Random.Set
import Set exposing (Set)


type alias Connection =
    ( Coordinate2D, Coordinate2D )


type alias ConnectionSet =
    Set Connection


type alias VisitedSet =
    Set Coordinate2D


type alias Stack =
    List Coordinate2D


type alias Record =
    { visited : VisitedSet
    , stack : Stack
    , connections : ConnectionSet
    , width : Int
    , height : Int
    }


defaultRecord : Record
defaultRecord =
    { visited = Set.fromList [ ( 0, 0 ) ]
    , stack = [ ( 0, 0 ) ]
    , connections = Set.empty
    , width = 1
    , height = 1
    }


type MazeGenerator
    = MazeGenerator Record


init : Int -> Int -> MazeGenerator
init width height =
    MazeGenerator { defaultRecord | width = width, height = height }


getTotalCellCount : Record -> Int
getTotalCellCount { width, height } =
    width * height


getVisitedCellCount : Record -> Int
getVisitedCellCount { visited } =
    Set.size visited


isSolvedRec : Record -> Bool
isSolvedRec rec =
    getVisitedCellCount rec == getTotalCellCount rec


isSolved : MazeGenerator -> Bool
isSolved (MazeGenerator rec) =
    isSolvedRec rec


getConnections : MazeGenerator -> ConnectionSet
getConnections (MazeGenerator rec) =
    rec.connections


step : Random.Seed -> MazeGenerator -> ( MazeGenerator, Random.Seed )
step seed (MazeGenerator rec) =
    (if isSolvedRec rec then
        ( rec, seed )

     else
        case rec.stack of
            c :: rest ->
                stepHelp seed c rec

            _ ->
                ( rec, seed )
    )
        |> Tuple.mapFirst MazeGenerator


solve : Random.Seed -> MazeGenerator -> ( MazeGenerator, Random.Seed )
solve seed mg =
    if isSolved mg then
        ( mg, seed )

    else
        step seed mg |> (\( a, b ) -> solve b a)


stepHelp : Random.Seed -> Coordinate2D -> Record -> ( Record, Random.Seed )
stepHelp seed stackTop rec =
    let
        isWithinBounds : Coordinate2D -> Bool
        isWithinBounds ( x, y ) =
            x >= 0 && y >= 0 && x < rec.width && y < rec.height

        isVisitedRec cord =
            Set.member cord rec.visited

        unVisitedNeighbours =
            stackTop
                |> Coordinate2D.perpendicularNeighboursOf
                |> List.filter isWithinBounds
                |> List.filter (isVisitedRec >> not)

        unVisitedNeighbourGenerator =
            unVisitedNeighbours
                |> Random.List.choose

        ( ( maybeCord, _ ), newSeed ) =
            Random.step unVisitedNeighbourGenerator seed
    in
    case maybeCord of
        Just cord ->
            ( { rec
                | visited = rec.visited |> Set.insert cord
                , stack = cord :: rec.stack
                , connections =
                    rec.connections
                        |> Set.insert ( stackTop, cord )
              }
            , newSeed
            )

        Nothing ->
            ( { rec | stack = rec.stack |> List.drop 1 }, newSeed )


getDimensions (MazeGenerator { width, height }) =
    { width = width, height = height }


getIsOnTopOfStack : Coordinate2D -> MazeGenerator -> Bool
getIsOnTopOfStack cord (MazeGenerator { stack }) =
    stack |> List.head |> Maybe.map (equals cord) |> Maybe.withDefault False


isVisitedCord cord (MazeGenerator { visited }) =
    Set.member cord visited


removeRandomConnections : Random.Seed -> MazeGenerator -> ( MazeGenerator, Random.Seed )
removeRandomConnections seed (MazeGenerator rec) =
    let
        newConnectionsGenerator : Random.Generator ConnectionSet
        newConnectionsGenerator =
            rec.connections
                |> Set.toList
                |> Random.List.shuffle
                |> Random.map (List.drop 10 >> Set.fromList)

        newVisitedGenerator : Random.Generator VisitedSet
        newVisitedGenerator =
            rec.visited
                |> Set.toList
                |> Random.List.shuffle
                |> Random.map (List.drop 10 >> Set.fromList)

        newRecGenerator : Random.Generator Record
        newRecGenerator =
            Random.map2 (\c v -> { rec | connections = c, visited = v })
                newConnectionsGenerator
                newVisitedGenerator
    in
    Random.step newRecGenerator seed
        |> Tuple.mapFirst MazeGenerator
