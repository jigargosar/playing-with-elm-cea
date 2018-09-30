module AMaze exposing (Maze, MazePath, dataGenerator, mazeHeight, mazeWidth)

import Array2D
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
    Array2D.Array2D MazePath


dataGenerator : Random.Generator (List (List MazePath))
dataGenerator =
    let
        pathGenerator : Random.Generator MazePath
        pathGenerator =
            Random.map2 MazePath Random.Extra.bool Random.Extra.bool

        rowGenerator =
            Random.list mazeWidth pathGenerator
    in
    Random.list mazeHeight rowGenerator
