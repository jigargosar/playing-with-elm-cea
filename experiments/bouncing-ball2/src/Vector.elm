module Vector exposing (Vector, polar, polarDegree, xy, zero)


type Vector
    = Cart Float Float
    | PolarRad Float Float
    | PolarDeg Float Float


xy =
    Cart


polar =
    PolarRad


polarDegree =
    PolarDeg


zero =
    xy
