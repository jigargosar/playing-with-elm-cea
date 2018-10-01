module Data2D exposing (Data2D, Data2DF, repeat)

import Array2D


type alias Data2D a =
    Array2D.Array2D a


type alias Data2DF a =
    Data2D a -> Data2D a


repeat : Int -> Int -> a -> Data2D a
repeat width height =
    Array2D.repeat height width
