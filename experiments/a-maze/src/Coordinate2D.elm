module Coordinate2D exposing (Coordinate2D, flatMap, map)


flatMap width height fn =
    map width height fn |> List.concat


map width height fn =
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
                    |> List.map (\x -> fn ( x, y ))
            )


type alias Coordinate2D =
    ( Int, Int )
