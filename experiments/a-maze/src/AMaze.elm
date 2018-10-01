module AMaze exposing (Maze, MazePath, dataGenerator, mazeHeight, mazeWidth)

import Array exposing (Array)
import Random
import Random.Array
import Random.Extra


mazeWidth =
    20


mazeHeight =
    25


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
