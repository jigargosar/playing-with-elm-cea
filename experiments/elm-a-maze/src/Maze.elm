module MazeGenerator
    exposing
        ( Maze
        )

import Coordinate2D exposing (Coordinate2D)
import MazeGenerator exposing (MazeGenerator)
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
        { mazeG = MazeGenerator.init mazeGSeed width height |> MazeGenerator.solve
        , seed = newSeed
        }


init : Random.Seed -> ( Int, Int ) -> MazeGenerator
init seed ( width, height ) =
    Maze (createRec seed width height)
