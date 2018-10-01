module AMaze exposing
    ( AMaze
    , MazeCellData
    , mapData
    , randomGenerator
    )

import Array exposing (Array)
import Ramda
import Random
import Random.Array
import Random.Extra


type alias MazeCellData =
    { down : Bool, right : Bool }


defaultCellData =
    MazeCellData False False


walledCellData =
    MazeCellData True True


withDefaultCellData =
    Maybe.withDefault defaultCellData


type alias AMaze =
    { width : Int, height : Int, data : MazeData }


type alias MazeData =
    Array (Array MazeCellData)


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



--walled : Int -> Int -> AMaze
--walled w h =
--    AMaze w h (Ramda.flatMapCoordinates2D w h (always walledCellData))


dataAt x y { data } =
    data |> Array.get y >> Maybe.andThen (Array.get x) >> withDefaultCellData


mapData : (Int -> Int -> MazeCellData -> a) -> AMaze -> List a
mapData fn m =
    Ramda.flatMapCoordinates2D m.width m.height (dataMapper fn m)


dataMapper fn m x y =
    fn x y (dataAt x y m)
