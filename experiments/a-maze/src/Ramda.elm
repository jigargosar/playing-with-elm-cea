module Ramda exposing (add, appendTo, flip, ifElse, iterateCoordinates2D, subBy, ter)


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


iterateCoordinates2D ( width, height ) fn =
    let
        xCords =
            List.range 0 (width - 1)

        yCords =
            List.range 0 (height - 1)
    in
    xCords
        |> List.map
            (\x ->
                yCords
                    |> List.map (\y -> fn (toFloat x) (toFloat y))
            )
        |> List.concat
