module NumPair exposing (..)

import PairA exposing (PairA)
import Ramda
import Ramda as R


type alias NumPair =
    PairA number


toFloat =
    R.mapBothWith Basics.toFloat


round =
    R.mapBothWith Basics.round
