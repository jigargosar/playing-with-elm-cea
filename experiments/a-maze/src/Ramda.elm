module Ramda exposing (add, appendTo, flip, ifElse, subBy, ter)


ter bool v1 v2 =
    if bool then
        v1

    else
        v2


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
