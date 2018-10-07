module PairA exposing (..)

import Ramda as R


type alias F a =
    a -> a


type alias PairA a =
    ( a, a )


type alias IntPair =
    PairA Int


type alias FloatPair =
    PairA Float


type alias StringPair =
    PairA String


toFloat : IntPair -> FloatPair
toFloat =
    map Basics.toFloat


round : FloatPair -> IntPair
round =
    map Basics.round


fromInt : IntPair -> StringPair
fromInt =
    map String.fromInt


fromIntWithSuffix : String -> IntPair -> StringPair
fromIntWithSuffix suf =
    fromInt >> withSuffix suf


withSuffix : String -> F StringPair
withSuffix suf =
    map (R.withSuffix suf)


type alias WH number =
    { w : number, h : number }


toWhRec : PairA number -> WH number
toWhRec ( w, h ) =
    WH w h


add offset =
    map ((+) offset)


mul factor =
    map ((*) factor)


iDivBy b a =
    a // b


iDiv factor =
    map (iDivBy factor)


map fn =
    Tuple.mapBoth fn fn


concatMapGrid : (IntPair -> a) -> IntPair -> List a
concatMapGrid fn cord =
    mapGrid fn cord |> List.concat


mapGrid : (IntPair -> a) -> IntPair -> List (List a)
mapGrid fn ( width, height ) =
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


addBoth ( a1, b1 ) ( a2, b2 ) =
    ( a1 + a2, b1 + b2 )


multiplyBoth ( a1, b1 ) ( a2, b2 ) =
    ( a1 * a2, b1 * b2 )
