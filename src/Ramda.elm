module Ramda exposing (equals, ifElse, when, whenEq)


ifElse pred t f v =
    if pred v then
        t v

    else
        f v


equals =
    (==)


when pred t v =
    if pred v then
        t v

    else
        v


whenEq v1 fn v2 =
    when (equals v1) fn v2
