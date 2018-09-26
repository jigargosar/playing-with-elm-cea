module Ramda exposing (eq, ifElse, when, whenEq)


ifElse pred t f v =
    if pred v then
        t v

    else
        f v


eq =
    (==)


when pred t v =
    if pred v then
        t v

    else
        v


whenEq v1 fn v2 =
    when (eq v1) fn v2
