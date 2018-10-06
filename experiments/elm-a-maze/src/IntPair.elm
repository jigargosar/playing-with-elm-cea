module IntPair
    exposing
        ( IntPair
        , concatMap
        , map
        , normalizeConnection
        , perpendicularNeighboursOf
        , scale
        , toString
        , translate
        , addBothPairs
        , multiplyBothPairs
        )


type alias IntPair =
    ( Int, Int )


concatMap : (IntPair -> a) -> IntPair -> List a
concatMap fn cord =
    map fn cord |> List.concat


map : (IntPair -> a) -> IntPair -> List (List a)
map fn ( width, height ) =
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


perpendicularNeighboursOf : IntPair -> List IntPair
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


addBothPairs ( a1, b1 ) ( a2, b2 ) =
    ( a1 + a2, b1 + b2 )


multiplyBothPairs ( a1, b1 ) ( a2, b2 ) =
    ( a1 * a2, b1 * b2 )
