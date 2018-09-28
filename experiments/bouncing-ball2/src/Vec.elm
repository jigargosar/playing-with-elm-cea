module Vec exposing (Vec, getX, getY, vec, vecAdd)


type Vec
    = Vec ( Float, Float )


vec : Float -> Float -> Vec
vec x y =
    Vec ( x, y )


vecAdd (Vec ( x, y )) (Vec ( x_, y_ )) =
    vec (x + x_) (y + y_)


getX (Vec ( x, y )) =
    x


getY (Vec ( x, y )) =
    y
