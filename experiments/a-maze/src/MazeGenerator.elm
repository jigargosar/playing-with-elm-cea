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

import Coordinate2D exposing (Coordinate2D)
import Ramda exposing (ensureAtLeast, equals)
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
    { visitedSet : VisitedSet
    , stack : Stack
    , connectionSet : ConnectionSet
    , width : Int
    , height : Int
    , seed : Random.Seed
    }


type MazeGenerator
    = MazeGenerator Record


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
            Random.step unVisitedNeighbourGenerator seed
    in
    case maybeCord of
        Just cord ->
            ( { rec
                | visitedSet = rec.visitedSet |> Set.insert cord
                , stack = cord :: rec.stack
                , connectionSet =
                    rec.connectionSet
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


isVisitedCord cord (MazeGenerator { visitedSet }) =
    Set.member cord visitedSet


removeRandomConnections : Random.Seed -> MazeGenerator -> ( MazeGenerator, Random.Seed )
removeRandomConnections seed (MazeGenerator rec) =
    let
        newConnectionsGenerator : Random.Generator ConnectionSet
        newConnectionsGenerator =
            rec.connectionSet
                |> Set.toList
                |> Random.List.shuffle
                |> Random.map (List.drop 10 >> Set.fromList)

        newVisitedGenerator : Random.Generator VisitedSet
        newVisitedGenerator =
            rec.visitedSet
                |> Set.toList
                |> Random.List.shuffle
                |> Random.map (List.drop 10 >> Set.fromList)

        newRecGenerator : Random.Generator Record
        newRecGenerator =
            Random.map2 (\c v -> { rec | connectionSet = c, visitedSet = v })
                newConnectionsGenerator
                newVisitedGenerator
    in
    Random.step newRecGenerator seed
        |> Tuple.mapFirst MazeGenerator


type alias CellInfo =
    { visitedSet : Bool, current : Bool }


concatMap : (Coordinate2D -> CellInfo -> a) -> MazeGenerator -> List a
concatMap fn mg =
    let
        { width, height } =
            getDimensions mg

        isVisited cord =
            isVisitedCord cord mg

        isOnTopOfStack cord =
            getIsOnTopOfStack cord mg

        mapper cord =
            fn cord { visitedSet = isVisited cord, current = isOnTopOfStack cord }
    in
    Coordinate2D.concatMap width height mapper


mapConnections : (Connection -> a) -> MazeGenerator -> List a
mapConnections fn mg =
    getConnections mg
        |> Set.toList
        |> List.map fn
