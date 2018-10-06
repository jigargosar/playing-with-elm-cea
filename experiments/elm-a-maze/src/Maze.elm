module Maze exposing (Maze, init, connected, concatMapCells)

import Coordinate2D exposing (Coordinate2D)
import MazeGenerator exposing (CellInfo, Connection, MazeGenerator)
import Ramda exposing (ensureAtLeast, equals)
import Random
import Random.Array
import Random.List
import Random.Set
import Set exposing (Set)


type alias Record =
    { mazeG : MazeGenerator
    , seed : Random.Seed
    }


type Maze
    = Maze Record


createRec : Random.Seed -> Int -> Int -> Record
createRec initialSeed width height =
    let
        ( mazeGSeed, newSeed ) =
            Random.step Random.independentSeed initialSeed
    in
        { mazeG = MazeGenerator.init mazeGSeed ( width, height ) |> MazeGenerator.solve
        , seed = newSeed
        }


init : Random.Seed -> ( Int, Int ) -> Maze
init seed ( width, height ) =
    Maze (createRec seed width height)


toRec (Maze r) =
    r


getMazeGen =
    toRec >> .mazeG


getSize =
    getMazeGen >> MazeGenerator.getSize


connected : Connection -> Maze -> Bool
connected connection =
    getMazeGen >> MazeGenerator.connected connection


concatMapCells : (Coordinate2D -> a) -> Maze -> List a
concatMapCells fn mg =
    Coordinate2D.concatMap fn (getSize mg)
