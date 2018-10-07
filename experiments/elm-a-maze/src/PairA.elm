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
    R.mapBothWith Basics.toFloat


round : FloatPair -> IntPair
round =
    R.mapBothWith Basics.round


fromInt : IntPair -> StringPair
fromInt =
    R.mapBothWith String.fromInt


fromIntWithSuffix : String -> IntPair -> StringPair
fromIntWithSuffix suf =
    fromInt >> withSuffix suf


withSuffix : String -> F StringPair
withSuffix suf =
    R.mapBothWith (R.withSuffix suf)


type alias Size number =
    { w : number, h : number }


toWhRec : PairA number -> Size number
toWhRec ( w, h ) =
    Size w h


add offset ( a1, a2 ) =
    ( a1 + offset, a2 + offset )


mul factor ( a1, a2 ) =
    ( a1 * factor, a2 * factor )


iDiv factor ( a1, a2 ) =
    ( a1 // factor, a2 // factor )


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


addBoth ( a1, b1 ) ( a2, b2 ) =
    ( a1 + a2, b1 + b2 )


multiplyBoth ( a1, b1 ) ( a2, b2 ) =
    ( a1 * a2, b1 * b2 )
