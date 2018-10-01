module AMaze exposing
    ( AMaze
    , MazeCellData
    , fillWalls
    , mapData
    , perpendicularNeighboursOf
    , randomGenerator
    , walled
    )

import Array exposing (Array)
import Ramda exposing (Coordinate2D)
import Random
import Random.Array
import Random.Extra


type alias MazeCellData =
    { down : Bool
    , right : Bool

    {- , top:Bool, left:Bool -}
    }


type alias AMaze =
    { width : Int, height : Int, data : MazeData }


type alias MazeData =
    Array (Array MazeCellData)


defaultCellData =
    MazeCellData False False


walledCellData =
    MazeCellData True True


createWalledMazeData : Int -> Int -> MazeData
createWalledMazeData w h =
    Array.repeat h (Array.repeat w walledCellData)


withDefaultCellData =
    Maybe.withDefault defaultCellData


dataGenerator : Int -> Int -> Random.Generator MazeData
dataGenerator w h =
    let
        pathGenerator : Random.Generator MazeCellData
        pathGenerator =
            Random.map2 MazeCellData Random.Extra.bool Random.Extra.bool

        rowGenerator =
            Random.Array.array w pathGenerator
    in
    Random.Array.array h rowGenerator


randomGenerator : Int -> Int -> Random.Generator AMaze
randomGenerator w h =
    Random.map (AMaze w h) (dataGenerator w h)


walled : Int -> Int -> AMaze
walled w h =
    AMaze w h (createWalledMazeData w h)


fillWalls : AMaze -> AMaze
fillWalls { width, height } =
    AMaze width height (createWalledMazeData width height)


dataAt : Coordinate2D -> AMaze -> MazeCellData
dataAt cord =
    maybeDataAtCord cord >> withDefaultCellData


maybeDataAtCord ( x, y ) { data } =
    data |> Array.get y >> Maybe.andThen (Array.get x)


mapData : (Coordinate2D -> MazeCellData -> a) -> AMaze -> List a
mapData fn m =
    Ramda.flatMapCoordinates2D m.width m.height (dataMapper fn m)


dataMapper : (Coordinate2D -> MazeCellData -> a) -> AMaze -> Coordinate2D -> a
dataMapper fn m cord =
    fn cord (dataAt cord m)


perpendicularNeighboursOf ( x, y ) m =
    1
