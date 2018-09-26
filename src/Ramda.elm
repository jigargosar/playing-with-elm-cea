module Ramda exposing (eqBy, equals, ifElse, when, whenEq)


ifElse pred t f v =
    if pred v then
        t v

    else
        f v


equals =
    (==)


eqBy f v1 v2 =
    f v1 == f v2


when pred t v =
    if pred v then
        t v

    else
        v


whenEq v1 fn v2 =
    when (equals v1) fn v2
