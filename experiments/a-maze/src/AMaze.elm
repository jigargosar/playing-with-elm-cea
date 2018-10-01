module AMaze exposing (AMaze, MazeCellData, defaultDataGenerator, mapData, mazeHeight, mazeWidth)

import Array exposing (Array)
import Ramda
import Random
import Random.Array
import Random.Extra


mazeWidth =
    18


mazeHeight =
    13


type alias MazeCellData =
    { down : Bool, right : Bool }


defaultCellData =
    MazeCellData False False


withDefaultCellData =
    Maybe.withDefault defaultCellData


type alias AMaze =
    { width : Int, height : Int, data : MazeData }


type alias MazeData =
    Array (Array MazeCellData)


defaultDataGenerator : Random.Generator MazeData
defaultDataGenerator =
    dataGenerator mazeWidth mazeHeight


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


mazeGenerator w h =
    Random.map (AMaze w h)


dataAt x y { data } =
    data |> Array.get y >> Maybe.andThen (Array.get x) >> withDefaultCellData


mapData : (Int -> Int -> MazeCellData -> a) -> AMaze -> List a
mapData fn m =
    Ramda.mapCoordinates2D m.width m.height (dataMapper fn m)


dataMapper fn m x y =
    fn x y (dataAt x y m)
