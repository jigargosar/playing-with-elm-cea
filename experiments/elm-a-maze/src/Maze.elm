module Maze exposing (Maze, init, connected, concatMapCells, isEastConnected, isSouthConnected)

import IntPair exposing (Int2)
import MazeGenerator exposing (CellInfo, Connection, ConnectionSet, MazeGenerator)
import PairA
import Ramda exposing (..)
import Random
import Random.Array
import Random.Extra
import Random.List
import Random.Pipeline as RP
import Random.Set
import Set exposing (Set)


type alias Record =
    { mazeG : MazeGenerator
    , moreConnections : ConnectionSet
    , seed : Random.Seed
    }


type Maze
    = Maze Record


cordGenerator : Int2 -> Random.Generator Int2
cordGenerator whPair =
    whPair |> PairA.add -2 |> PairA.map (Random.int 0) |> uncurry Random.pair


eastConnectionGenerator =
    cordGenerator >> Random.map eastConnection


southConnectionGenerator =
    cordGenerator >> Random.map southConnection


connectionGenerator : Int2 -> Random.Generator Connection
connectionGenerator whPair =
    Random.Extra.choices (eastConnectionGenerator whPair) [ southConnectionGenerator whPair ]


connectionSetGenerator : Int2 -> Random.Generator ConnectionSet
connectionSetGenerator whI2 =
    let
        num =
            150

        eliminateAndCombine cs =
            Random.Set.notInSet cs (connectionGenerator whI2)
                |> Random.Set.set num
                |> Random.map (Set.union cs)
    in
        connectionGenerator whI2
            |> Random.Set.set num



--            |> Random.andThen eliminateAndCombine


mazeGenerator sizePair =
    Random.independentSeed |> Random.map (\seed -> MazeGenerator.init seed sizePair |> MazeGenerator.solve)


createRec : Random.Seed -> Int2 -> Record
createRec seed sizePair =
    Record
        |> RP.from seed
        |> RP.with (mazeGenerator sizePair)
        |> RP.with (connectionSetGenerator sizePair)
        --        |> RP.always Set.empty
        |> RP.finish


init : Random.Seed -> Int2 -> Maze
init seed sizePair =
    Maze (createRec seed sizePair)


toRec (Maze r) =
    r


getMazeG =
    toRec >> .mazeG


getMoreConnection =
    toRec >> .moreConnections


getSize =
    getMazeG >> MazeGenerator.getSize


mazeGConnected : Connection -> Maze -> Bool
mazeGConnected connection =
    getMazeG >> MazeGenerator.connected connection


moreConnected : Connection -> Maze -> Bool
moreConnected connection =
    getMoreConnection >> Set.member (IntPair.normalizeConnection connection)


connected : Connection -> Maze -> Bool
connected connection maze =
    moreConnected connection maze || mazeGConnected connection maze


concatMapCells : (Int2 -> a) -> Maze -> List a
concatMapCells fn =
    getSize >> IntPair.concatMap fn


isSouthConnected cord =
    connected (southConnection cord)


isEastConnected cord =
    connected (eastConnection cord)


eastConnection ( x, y ) =
    ( ( x, y ), ( x + 1, y ) )


southConnection ( x, y ) =
    ( ( x, y ), ( x, y + 1 ) )
