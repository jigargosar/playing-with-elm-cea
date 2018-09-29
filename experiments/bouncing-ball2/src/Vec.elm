module Vec exposing
    ( Vec
    , add
    , div
    , fromInt
    , fromPair
    , fromRec
    , getX
    , getY
    , newMA
    , newXY
    , sub
    , toMA
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
        angle =
            degrees deg
    in
    Vec ( m * cos angle, m * sin angle )


toMA (Vec ( x, y )) =
    ( sqrt (x ^ 2 + y ^ 2), atan (y / x) )


zero =
    newXY 0 0


fromInt : Int -> Int -> Vec
fromInt x y =
    Vec ( toFloat x, toFloat y )


add (Vec ( x, y )) (Vec ( x_, y_ )) =
    newXY (x + x_) (y + y_)


sub (Vec ( x, y )) (Vec ( x_, y_ )) =
    newXY (x - x_) (y - y_)


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


fromRec { x, y } =
    Vec (x y)


fromPair =
    Vec
