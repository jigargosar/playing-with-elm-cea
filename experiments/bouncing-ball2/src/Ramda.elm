module Ramda exposing (add, flip, subBy, ter)


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
