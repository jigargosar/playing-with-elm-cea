module BasicsX exposing (vec2FromPair, vec2ToPair)

import Math.Vector2 as V exposing (Vec2)


vec2FromPair ( x, y ) =
    V.vec2 x y


vec2ToPair =
    V.toRecord >> (\{ x, y } -> ( x, y ))
