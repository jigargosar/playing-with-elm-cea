module Maze exposing (Maze, init, connected, concatMapCells, isEastConnected, isSouthConnected)

import IntPair exposing (IntPair)
import MazeGenerator exposing (CellInfo, Connection, ConnectionSet, MazeGenerator)
import PairA
import Ramda exposing (..)
import Random
import Random.Array
import Random.Extra
import Random.List
import Random.Set
import Set exposing (Set)


type alias Record =
    { mazeG : MazeGenerator
    , seed : Random.Seed
    , moreConnections : ConnectionSet
    }


type Maze
    = Maze Record


cordGenerator : IntPair -> Random.Generator IntPair
cordGenerator whPair =
    whPair |> PairA.add -1 |> PairA.map (Random.int 0) |> uncurry Random.pair


eastConnectionGenerator =
    cordGenerator >> Random.map eastConnection


southConnectionGenerator =
    cordGenerator >> Random.map southConnection


connectionGenerator : IntPair -> Random.Generator Connection
connectionGenerator whPair =
    Random.Extra.choices (eastConnectionGenerator whPair) [ southConnection whPair ]


createRec : Random.Seed -> Int -> Int -> Record
createRec initialSeed width height =
    let
        ( mazeGSeed, newSeed ) =
            Random.step Random.independentSeed initialSeed
    in
        { mazeG = MazeGenerator.init mazeGSeed ( width, height ) |> MazeGenerator.solve
        , seed = newSeed
        , moreConnections = Set.empty
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


concatMapCells : (IntPair -> a) -> Maze -> List a
concatMapCells fn =
    getSize >> IntPair.concatMap fn


isSouthConnected cord =
    connected (eastConnection cord)


isEastConnected cord =
    connected (eastConnection cord)


eastConnection ( x, y ) =
    ( ( x, y ), ( x + 1, y ) )


southConnection ( x, y ) =
    ( ( x, y ), ( x, y + 1 ) )
