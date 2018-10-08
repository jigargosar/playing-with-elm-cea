module PairA exposing (..)

import Ramda as R


type alias F a =
    a -> a


type alias PairA a =
    ( a, a )


pair =
    Tuple.pair


zero =
    pair 0 0


type alias IntPair =
    PairA Int


type alias FloatPair =
    PairA Float


type alias StringPair =
    PairA String


map fn =
    Tuple.mapBoth fn fn


floatFromInt : IntPair -> FloatPair
floatFromInt =
    map Basics.toFloat


round : FloatPair -> IntPair
round =
    map Basics.round


stringFromInt : IntPair -> StringPair
stringFromInt =
    map String.fromInt


stringFromIntWithSuffix : String -> IntPair -> StringPair
stringFromIntWithSuffix suf =
    stringFromInt >> withSuffix suf


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


iDiv factor =
    map (R.flip (//) factor)


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


add2 =
    apply2 (+)


mul2 =
    apply2 (*)


apply2 fn ( a1, b1 ) ( a2, b2 ) =
    ( fn a1 a2, fn b1 b2 )
