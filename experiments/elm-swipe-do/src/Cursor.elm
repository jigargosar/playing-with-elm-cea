module Cursor exposing (Cursor, get)


type alias Cursor =
    Int


get : List a -> Cursor -> ( Cursor, List a )
get list cursor =
    let
        newCursor =
            clamp 0 (List.length list) cursor
    in
    ( newCursor, list )
