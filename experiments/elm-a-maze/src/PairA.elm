module PairA exposing (..)

import Basics.Extra
import Ramda as R


type alias F a =
    a -> a


type alias PairA a =
    ( a, a )


pair =
    Tuple.pair


zero =
    pair 0 0


type alias Int2 =
    PairA Int


type alias Float2 =
    PairA Float


type alias StringPair =
    PairA String


map fn =
    Tuple.mapBoth fn fn


map2 fn ( a1, b1 ) ( a2, b2 ) =
    ( fn a1 a2, fn b1 b2 )


apply =
    Basics.Extra.uncurry


toFloat : Int2 -> Float2
toFloat =
    map Basics.toFloat


round : Float2 -> Int2
round =
    map Basics.round


fromInt : Int2 -> StringPair
fromInt =
    map String.fromInt


fromIntThenSuffix : String -> Int2 -> StringPair
fromIntThenSuffix suf =
    fromInt >> suffix suf


suffix : String -> F StringPair
suffix suf =
    map (R.suffix suf)


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


concatMapGrid : (Int2 -> a) -> Int2 -> List a
concatMapGrid fn cord =
    mapGrid fn cord |> List.concat


mapGrid : (Int2 -> a) -> Int2 -> List (List a)
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


sum =
    map2 (+)


product =
    map2 (*)
