module RGBA exposing (RGBA, fromPartial, rgba, toHexAString)

import Hex
import Round


type alias RGBA =
    { red : Float, green : Float, blue : Float, alpha : Float }


type alias PartialRGBA a =
    { a | red : Float, green : Float, blue : Float, alpha : Float }


toHexAString : RGBA -> String
toHexAString color =
    let
        { red, green, blue, alpha } =
            color
    in
    [ red, green, blue, alpha ]
        |> List.map
            (clamp 0 1
                >> (*) 255
                >> Round.truncate
                >> Hex.toString
                >> String.padLeft 2 '0'
            )
        |> (::) "#"
        |> String.join ""


rgba : Float -> Float -> Float -> Float -> RGBA
rgba r g b a =
    { red = r, green = g, blue = b, alpha = a }


fromPartial : PartialRGBA a -> RGBA
fromPartial ({ red, green, blue, alpha } as partial) =
    RGBA red green blue alpha
