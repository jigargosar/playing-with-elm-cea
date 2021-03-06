module Hsla exposing (Fn, HSLA, create, fromPartial, hueInt, lightnessInt, saturationInt, toHexAString, toRGBA)

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


hueInt : HSLA -> Int
hueInt =
    .hue >> (*) 359 >> Round.truncate


saturationInt : HSLA -> Int
saturationInt =
    .saturation >> (*) 100 >> Round.truncate


lightnessInt : HSLA -> Int
lightnessInt =
    .saturation >> (*) 100 >> Round.truncate
