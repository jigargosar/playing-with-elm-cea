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
