module Data2D exposing (Data2D, Data2DF, perpendicularNeighboursOf, repeat)

import Array2D exposing (Array2D)
import Coordinate2D exposing (Coordinate2D)
import Ramda exposing (flip)


type Data2D a
    = Data2D (Array2D a)


type alias Cell a =
    ( Coordinate2D, a )


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


getIn : Data2D a -> Coordinate2D -> Maybe a
getIn =
    flip get


perpendicularNeighboursOf cord data2D =
    Coordinate2D.perpendicularNeighboursOf cord
        |> List.map (getIn data2D)
