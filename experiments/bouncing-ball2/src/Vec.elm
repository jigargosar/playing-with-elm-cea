module Vec exposing
    ( Vec
    , add
    , div
    , fromInt
    , getX
    , getY
    , newMA
    , newXY
    , toPair
    , toRec
    , zero
    )

import Tuple2


type Vec
    = Vec ( Float, Float )


newXY : Float -> Float -> Vec
newXY x y =
    Vec ( x, y )


newMA : Float -> Float -> Vec
newMA m deg =
    let
        rad =
            radians deg
    in
    Vec ( m * sin rad, m * cos rad )


zero =
    newXY 0 0


fromInt : Int -> Int -> Vec
fromInt x y =
    Vec ( toFloat x, toFloat y )


add (Vec ( x, y )) (Vec ( x_, y_ )) =
    newXY (x + x_) (y + y_)


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
