module MazeGenerator exposing (MazeGenerator, init)

import Coordinate2D exposing (Coordinate2D)
import Random
import Random.List
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


isSolved : Record -> Bool
isSolved rec =
    getVisitedCellCount rec == getTotalCellCount rec


step : Random.Seed -> MazeGenerator -> ( MazeGenerator, Random.Seed )
step seed (MazeGenerator rec) =
    (if isSolved rec then
        ( rec, seed )

     else
        case rec.stack of
            c :: rest ->
                stepHelp seed c rec

            _ ->
                ( rec, seed )
    )
        |> Tuple.mapFirst MazeGenerator


stepHelp : Random.Seed -> Coordinate2D -> Record -> ( Record, Random.Seed )
stepHelp seed stackTop rec =
    let
        isWithinBounds : Coordinate2D -> Bool
        isWithinBounds ( x, y ) =
            x >= 0 && y >= 0 && x < rec.width && y < rec.height

        isVisited cord =
            Set.member cord rec.visited

        _ =
            ( getVisitedCellCount rec, getTotalCellCount rec ) |> Debug.log "(visited,total)"

        unVisitedNeighbours =
            stackTop
                |> Coordinate2D.perpendicularNeighboursOf
                |> List.filter isWithinBounds
                |> List.filter (isVisited >> not)

        unVisitedNeighbourGenerator =
            unVisitedNeighbours
                |> Debug.log "unVisitedNeighbours"
                |> Random.List.choose

        ( ( maybeCord, _ ), newSeed ) =
            Random.step unVisitedNeighbourGenerator seed

        _ =
            maybeCord |> Debug.log "randomCord"
    in
    case maybeCord of
        Just cord ->
            let
                newVisitedCords =
                    rec.visited |> Set.insert cord

                newConnections =
                    rec.connections
                        |> Set.insert ( stackTop, cord )
            in
            ( { rec
                | visited = newVisitedCords
                , stack = cord :: rec.stack
                , connections = newConnections
              }
            , newSeed
            )

        Nothing ->
            ( { rec | stack = rec.stack |> List.drop 1 }, newSeed )
