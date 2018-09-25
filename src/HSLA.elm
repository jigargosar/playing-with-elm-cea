module HSLA exposing (HSLA, create, fromPartial, toHexAString)

import Color
import InternalColor
import RGBA


type alias HSLA =
    InternalColor.HSLA


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
    toRGBA >> RGBA.toHexAString
