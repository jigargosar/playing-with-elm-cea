module Coordinate2D exposing (Coordinate2D, flatMapCoordinates2D, mapCoordinates2D)


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
                    |> List.map (\x -> fn ( x, y ))
            )


type alias Coordinate2D =
    ( Int, Int )
