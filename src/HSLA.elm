module HSLA exposing (HSLA, fromPartial, hsla, toHexAString)

import Color
import Hex
import InternalColor
import RGBA
import Round


hsla : Float -> Float -> Float -> Float -> HSLA
hsla =
    InternalColor.HSLA


type alias HSLA =
    hsla


type alias PartialHSLA a =
    { a | hue : Float, saturation : Float, lightness : Float, alpha : Float }


fromPartial : PartialHSLA a -> HSLA
fromPartial { hue, saturation, lightness, alpha } =
    hsla hue saturation lightness alpha


toRGBA : HSLA -> InternalColor.RGBA
toRGBA =
    Color.fromHsla >> Color.toRgba


toHexAString : HSLA -> String
toHexAString =
    toRGBA >> RGBA.toHexAString
