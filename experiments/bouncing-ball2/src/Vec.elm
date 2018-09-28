module Vec exposing (Vec, add, div, fromInt, getX, getY, toPair, toRec, vec)

import Tuple2


type Vec
    = Vec ( Float, Float )


vec : Float -> Float -> Vec
vec x y =
    Vec ( x, y )


fromInt : Int -> Int -> Vec
fromInt x y =
    Vec ( toFloat x, toFloat y )


add (Vec ( x, y )) (Vec ( x_, y_ )) =
    vec (x + x_) (y + y_)


div s (Vec p) =
    p |> Tuple2.mapBoth (divBy s) >> Vec


getX (Vec ( x, y )) =
    x


getY (Vec ( x, y )) =
    y


toRec (Vec ( x, y )) =
    { x = x, y = y }


toPair (Vec p) =
    p


divBy b a =
    a / b
