module NumPair exposing (..)

import Ramda
import Ramda as R


type alias PairA a =
    ( a, a )


type alias NumPair =
    PairA number


toFloat =
    R.mapBothWith Basics.toFloat


round =
    R.mapBothWith Basics.round
