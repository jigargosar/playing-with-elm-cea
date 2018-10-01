module MazeGenerator exposing (MazeGenerator, init)

import Coordinate2D exposing (Coordinate2D)
import Random
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


step : MazeGenerator -> MazeGenerator
step (MazeGenerator rec) =
    (if rec.visited.size == rec.width * rec.height then
        rec

     else
        case rec.stack of
            c :: rest ->
                stepHelp c rec

            _ ->
                rec
    )
        |> MazeGenerator


stepHelp : Record -> Record
stepHelp stackTop rec =
    let
        totalCellCount =
            rec.width * rec.height

        _ =
            ( Set.size rec.visited, totalCellCount ) |> Debug.log "(visited,total)"

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
