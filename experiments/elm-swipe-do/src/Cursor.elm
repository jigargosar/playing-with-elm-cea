module Cursor exposing (Cursor, cycleByOffset, get)


type alias Cursor =
    Int


get : List a -> Cursor -> ( Cursor, List a )
get list cursor =
    let
        newCursor =
            clamp 0 (List.length list - 1) cursor
    in
    ( newCursor, list )


cycleByOffset offset list cursor =
    let
        length =
            List.length list
    in
    if length == 0 then
        cursor

    else
        modBy length (cursor + offset)
