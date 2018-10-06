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


suffix : String -> F StringPair
suffix suf =
    R.mapBothWith (R.suffix suf)
