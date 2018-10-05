module Coordinate2D exposing
    ( Coordinate2D
    , concatMap
    , map
    , normalizeConnection
    , perpendicularNeighboursOf
    , scale
    , toString
    , translate
    )


type alias Coordinate2D =
    ( Int, Int )


concatMap width height fn =
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


scale s ( x, y ) =
    ( s * x, s * y )


translate t ( x, y ) =
    ( t + x, t + y )


toString ( x, y ) =
    [ "("
    , String.fromInt x
    , ","
    , String.fromInt y
    , ")"
    ]
        |> String.join ""


normalizeConnection ( c1, c2 ) =
    let
        ( x1, y1 ) =
            c1

        ( x2, y2 ) =
            c2
    in
    if x1 > x2 || y1 > y2 then
        ( c2, c1 )

    else
        ( c1, c2 )
