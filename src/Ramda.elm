module Ramda exposing (eqBy, equals, ifElse, unless, when, whenEq)


ifElse pred t f v =
    if pred v then
        t v

    else
        f v


equals =
    (==)


eqBy fn v1 v2 =
    fn v1 == fn v2


when pred fn v =
    if pred v then
        fn v

    else
        v


unless pred fn v =
    if pred v then
        v

    else
        fn v


whenEq v1 fn v2 =
    when (equals v1) fn v2
