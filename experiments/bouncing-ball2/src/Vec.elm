module Vec exposing (Vec, add, div, fromInt, getX, getY, new, toPair, toRec, zero)

import Tuple2


type Vec
    = Vec ( Float, Float )


new : Float -> Float -> Vec
new x y =
    Vec ( x, y )


zero =
    new 0 0


fromInt : Int -> Int -> Vec
fromInt x y =
    Vec ( toFloat x, toFloat y )


add (Vec ( x, y )) (Vec ( x_, y_ )) =
    new (x + x_) (y + y_)


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
