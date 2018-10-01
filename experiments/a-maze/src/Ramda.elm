module Ramda exposing
    ( add
    , appendTo
    , equals
    , flatMapCoordinates2D
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


flatMapCoordinates2D width height fn =
    mapCoordinates2D width height fn |> List.concat


mapCoordinates2D width height fn =
    let
        xCords =
            List.range 0 (width - 1)

        yCords =
            List.range 0 (height - 1)
    in
    yCords
        |> List.map
            (\y ->
                xCords
                    |> List.map (\x -> fn x y)
            )
