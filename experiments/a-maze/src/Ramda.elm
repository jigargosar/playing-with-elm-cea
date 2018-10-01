module Ramda exposing
    ( add
    , appendTo
    , equals
    , flip
    , ifElse
    , isEmptyList
    , subBy
    , ter
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
