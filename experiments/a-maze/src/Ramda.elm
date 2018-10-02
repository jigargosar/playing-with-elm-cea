module Ramda exposing
    ( add
    , appendTo
    , ensureAtLeast
    , equals
    , flip
    , ifElse
    , isEmptyList
    , subBy
    , ter
    , unless
    , when
    )


isEmptyList =
    List.length >> equals 0


ter bool v1 v2 =
    if bool then
        v1

    else
        v2


equals =
    (==)


flip fn a b =
    fn b a


subBy =
    flip (-)


add =
    (+)


appendTo =
    flip (::)


ifElse pred true false value =
    if pred value then
        true value

    else
        false value


when : (a -> Bool) -> (a -> a) -> a -> a
when pred true value =
    ifElse pred true identity value



{- if pred value then
       true value

   else
       value
-}


unless pred =
    when (pred >> not)


ensureAtLeast : comparable -> comparable -> comparable
ensureAtLeast =
    max


swap ( a, b ) =
    ( b, a )
