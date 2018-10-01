module AMaze exposing (Maze, MazePath, dataAt, dataGenerator, mazeHeight, mazeWidth)

import Array exposing (Array)
import Random
import Random.Array
import Random.Extra


mazeWidth =
    3


mazeHeight =
    5


type alias MazePath =
    { down : Bool, right : Bool }


type alias Maze =
    { width : Int, height : Int, data : MazeData }


type alias MazeData =
    Array (Array MazePath)


dataGenerator : Random.Generator (Array (Array MazePath))
dataGenerator =
    let
        pathGenerator : Random.Generator MazePath
        pathGenerator =
            Random.map2 MazePath Random.Extra.bool Random.Extra.bool

        rowGenerator =
            Random.Array.array mazeWidth pathGenerator
    in
    Random.Array.array mazeHeight rowGenerator


dataAt x y { data } =
    data |> Array.get y |> Maybe.andThen (Array.get x)
