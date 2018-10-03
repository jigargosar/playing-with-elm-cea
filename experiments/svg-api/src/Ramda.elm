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
        , mapT
        , scale
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



{- if pred value then
       true value

   else
       value
-}


unless : (a -> Bool) -> (a -> a) -> a -> a
unless pred =
    when (pred >> not)


ensureAtLeast : comparable -> comparable -> comparable
ensureAtLeast =
    max


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


mapT fn =
    Tuple.mapBoth fn fn
