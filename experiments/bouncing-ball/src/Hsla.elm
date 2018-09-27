module Hsla exposing (Fn, HSLA, create, fromPartial, hueAsInt, toHexAString, toRGBA)

import Color
import InternalColor
import Rgba
import Round


type alias HSLA =
    InternalColor.HSLA


type alias Fn =
    HSLA -> HSLA


type alias PartialHSLA a =
    { a | hue : Float, saturation : Float, lightness : Float, alpha : Float }


create : Float -> Float -> Float -> Float -> HSLA
create =
    InternalColor.HSLA


fromPartial : PartialHSLA a -> HSLA
fromPartial { hue, saturation, lightness, alpha } =
    create hue saturation lightness alpha


toRGBA : HSLA -> InternalColor.RGBA
toRGBA =
    Color.fromHsla >> Color.toRgba


toHexAString : HSLA -> String
toHexAString =
    toRGBA >> Rgba.toHexAString


hueAsInt : HSLA -> Int
hueAsInt =
    .hue >> (*) 359 >> Round.truncate
