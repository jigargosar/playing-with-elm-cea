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
    Random.Extra.choices (eastConnectionGenerator whPair) [ southConnectionGenerator whPair ]


connectionSetGenerator : IntPair -> Random.Generator ConnectionSet
connectionSetGenerator =
    connectionGenerator >> Random.Set.set 20


mazeGenerator sizePair =
    Random.independentSeed |> Random.map (\seed -> MazeGenerator.init seed sizePair |> MazeGenerator.solve)


createRec : Random.Seed -> IntPair -> Record
createRec initialSeed sizePair =
    let
        ( mazeG, newSeed ) =
            Random.step (mazeGenerator sizePair) initialSeed
    in
        { mazeG = mazeG
        , seed = newSeed
        , moreConnections = Set.empty
        }


init : Random.Seed -> IntPair -> Maze
init seed sizePair =
    Maze (createRec seed sizePair)


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
