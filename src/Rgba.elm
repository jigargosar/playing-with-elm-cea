module Rgba exposing (Fn, RGBA, create, fromPartial, to255A, toHSLA, toHexAString)

import Color
import Hex
import InternalColor
import Round


type alias RGBA =
    InternalColor.RGBA


type alias RGBA255 =
    { red : Int, green : Int, blue : Int, alpha : Float }


type alias PartialRGBA a =
    { a | red : Float, green : Float, blue : Float, alpha : Float }


type alias Fn =
    RGBA -> RGBA


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


create : Float -> Float -> Float -> Float -> RGBA
create =
    InternalColor.RGBA


fromPartial : PartialRGBA a -> RGBA
fromPartial { red, green, blue, alpha } =
    create red green blue alpha


toHSLA : RGBA -> InternalColor.HSLA
toHSLA =
    Color.fromRgba >> Color.toHsla


to255A : RGBA -> RGBA255
to255A { red, green, blue, alpha } =
    let
        to255 =
            (*) 255.0 >> Round.truncate
    in
    RGBA255 (to255 red) (to255 green) (to255 blue) alpha
