module Tuple2 exposing (mapBoth, mapEach)


mapBoth f =
    Tuple.mapBoth f f


mapEach fa fb =
    Tuple.mapBoth fa fb
