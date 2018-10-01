module Coordinate2D exposing (Coordinate2D, flatMap, map, perpendicularNeighboursOf)


type alias Coordinate2D =
    ( Int, Int )


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


perpendicularNeighboursOf : Coordinate2D -> List Coordinate2D
perpendicularNeighboursOf ( x, y ) =
    [ ( x + 1, y )
    , ( x - 1, y )
    , ( x, y + 1 )
    , ( x, y - 1 )
    ]
