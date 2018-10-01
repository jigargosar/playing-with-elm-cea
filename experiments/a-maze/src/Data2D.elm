module Data2D exposing (Data2D, Data2DF, repeat)

import Array2D exposing (Array2D)
import Coordinate2D exposing (Coordinate2D)


type Data2D a
    = Data2D (Array2D a)


type alias Data2DF a =
    Data2D a -> Data2D a


type alias Array2DF a =
    Array2D a -> Array2D a


unwrap fn (Data2D a2) =
    fn a2



--mapWrap: Data2DF
--mapWrap fn =
--    unwrap fn >> Data2D


repeat : Int -> Int -> a -> Data2D a
repeat width height =
    Array2D.repeat height width >> Data2D


get : Coordinate2D -> Data2D a -> Maybe a
get ( x, y ) =
    unwrap (Array2D.get y x)


pNeighbours cord (Data2D a2) =
    Array2D.get
