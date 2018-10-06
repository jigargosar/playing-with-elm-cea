module MazeGenerator
    exposing
        ( CellInfo
        , Connection
        , MazeGenerator
        , MazeGeneratorF
        , concatMapCellInfo
        , concatMapCords
        , init
        , isSolved
        , mapConnections
        , reset
        , solve
        , step
        , connected
        , getSize
        )

import IntPair exposing (IntPair)
import Ramda exposing (ensureAtLeast, equals)
import Random
import Random.Array
import Random.List
import Random.Set
import Set exposing (Set)


type alias Connection =
    ( IntPair, IntPair )


type alias ConnectionSet =
    Set Connection


type alias VisitedSet =
    Set IntPair


type alias Stack =
    List IntPair


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


createRec : Random.Seed -> Int -> Int -> Record
createRec seed width height =
    { visitedSet = Set.fromList [ ( 0, 0 ) ]
    , stack = [ ( 0, 0 ) ]
    , connectionSet = Set.empty
    , width = ensureAtLeast 1 width
    , height = ensureAtLeast 1 height
    , seed = seed
    }


init : Random.Seed -> ( Int, Int ) -> MazeGenerator
init seed ( width, height ) =
    MazeGenerator (createRec seed width height)


getSize (MazeGenerator rec) =
    ( rec.width, rec.height )


reset : MazeGeneratorF
reset (MazeGenerator { seed, width, height }) =
    MazeGenerator (createRec seed width height)


getTotalCellCount : Record -> Int
getTotalCellCount { width, height } =
    width * height


getVisitedCellCount : Record -> Int
getVisitedCellCount { visitedSet } =
    Set.size visitedSet


isSolvedRec : Record -> Bool
isSolvedRec rec =
    getVisitedCellCount rec >= getTotalCellCount rec


isSolved : MazeGenerator -> Bool
isSolved (MazeGenerator rec) =
    isSolvedRec rec


getConnections : MazeGenerator -> ConnectionSet
getConnections (MazeGenerator rec) =
    rec.connectionSet


addConnection : Connection -> Record -> ConnectionSet
addConnection connection rec =
    rec.connectionSet |> Set.insert (IntPair.normalizeConnection connection)


connected : Connection -> MazeGenerator -> Bool
connected connection =
    getConnections >> Set.member (IntPair.normalizeConnection connection)


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


stepHelp : IntPair -> Record -> Record
stepHelp stackTop rec =
    let
        isWithinBounds : IntPair -> Bool
        isWithinBounds ( x, y ) =
            x >= 0 && y >= 0 && x < rec.width && y < rec.height

        isVisitedRec cord =
            Set.member cord rec.visitedSet

        unVisitedNeighbours =
            stackTop
                |> IntPair.perpendicularNeighboursOf
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
                        addConnection ( stackTop, cord ) rec
                }

            Nothing ->
                { rec
                    | seed = newSeed
                    , stack = rec.stack |> List.drop 1
                }


getDimensions (MazeGenerator { width, height }) =
    { width = width, height = height }


isCordOnTopOfStack : IntPair -> MazeGenerator -> Bool
isCordOnTopOfStack cord (MazeGenerator { stack }) =
    stack |> List.head |> Maybe.map (equals cord) |> Maybe.withDefault False


isCordVisited cord (MazeGenerator { visitedSet }) =
    Set.member cord visitedSet


type alias CellInfo =
    { visited : Bool, current : Bool }


concatMapCellInfo : (IntPair -> CellInfo -> a) -> MazeGenerator -> List a
concatMapCellInfo fn mg =
    let
        mapper cord =
            fn cord
                { visited = isCordVisited cord mg
                , current = isCordOnTopOfStack cord mg
                }
    in
        (getSize mg) |> IntPair.concatMap mapper


concatMapCords : (IntPair -> a) -> MazeGenerator -> List a
concatMapCords fn =
    getSize >> IntPair.concatMap fn


mapConnections : (Connection -> a) -> MazeGenerator -> List a
mapConnections fn mg =
    getConnections mg
        |> Set.toList
        |> List.map fn
