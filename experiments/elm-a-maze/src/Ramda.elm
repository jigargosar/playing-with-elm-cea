module Ramda
    exposing
        ( add
        , consTo
        , ensureAtLeast
        , equals
        , flip
        , ifElse
        , isListEmpty
        , subBy
        , ter
        , unless
        , when
        , mapBothWith
        , scale
        , tupleToList
        , toTuple
        )


isListEmpty =
    List.length >> equals 0


ter : Bool -> a -> a -> a
ter bool v1 v2 =
    if bool then
        v1
    else
        v2


scale =
    (*)


equals : a -> a -> Bool
equals =
    (==)


flip : (a -> b -> c) -> b -> a -> c
flip fn b a =
    fn a b


subBy : number -> number -> number
subBy =
    flip (-)


add : number -> number -> number
add =
    (+)


consTo : List a -> a -> List a
consTo =
    flip (::)


ifElse : (v -> Bool) -> (v -> a) -> (v -> a) -> v -> a
ifElse pred true false value =
    ter (pred value) (true value) (false value)


when : (a -> Bool) -> (a -> a) -> a -> a
when pred true value =
    ifElse pred true identity value


unless : (a -> Bool) -> (a -> a) -> a -> a
unless pred =
    when (pred >> not)


ensureAtLeast : comparable -> comparable -> comparable
ensureAtLeast =
    max


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


mapBothWith fn =
    Tuple.mapBoth fn fn


tupleToList : ( a, a ) -> List a
tupleToList ( a, b ) =
    [ a, b ]


toTuple a =
    Tuple.pair a a
