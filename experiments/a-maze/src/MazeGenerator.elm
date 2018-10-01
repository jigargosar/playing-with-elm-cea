module MazeGenerator exposing (MazeGenerator, init)

import Coordinate2D exposing (Coordinate2D)
import Random
import Set exposing (Set)


type alias Connection =
    ( Coordinate2D, Coordinate2D )


type alias ConnectionSet =
    Set Connection


type alias VisitedSet =
    Set Coordinate2D


type alias Stack =
    List Coordinate2D


type alias Record =
    { visited : VisitedSet
    , stack : Stack
    , connections : ConnectionSet
    , width : Int
    , height : Int
    }


defaultRecord : Record
defaultRecord =
    { visited = Set.fromList [ ( 0, 0 ) ]
    , stack = [ ( 0, 0 ) ]
    , connections = Set.empty
    , width = 1
    , height = 1
    }


type MazeGenerator
    = MazeGenerator Record


init : Int -> Int -> MazeGenerator
init width height =
    MazeGenerator { defaultRecord | width = width, height = height }
