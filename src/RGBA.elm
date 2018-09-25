module RGBA exposing (RGBA, toHexA)

import Hex
import Round


type alias RGBA =
    { red : Float, green : Float, blue : Float, alpha : Float }


toHexA : RGBA -> String
toHexA color =
    let
        { red, green, blue, alpha } =
            color
    in
    [ red, green, blue, alpha ]
        |> List.map (clamp 0 1 >> (*) 255 >> Round.truncate >> Hex.toString)
        |> (::) "#"
        |> String.join ""
