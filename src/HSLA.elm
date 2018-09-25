module HSLA exposing (HSLA, fromPartial, hsla, toHexAString)

import Color
import InternalColor
import RGBA


type alias HSLA =
    InternalColor.HSLA


type alias PartialHSLA a =
    { a | hue : Float, saturation : Float, lightness : Float, alpha : Float }


hsla : Float -> Float -> Float -> Float -> HSLA
hsla =
    InternalColor.HSLA


fromPartial : PartialHSLA a -> HSLA
fromPartial { hue, saturation, lightness, alpha } =
    hsla hue saturation lightness alpha


toRGBA : HSLA -> InternalColor.RGBA
toRGBA =
    Color.fromHsla >> Color.toRgba


toHexAString : HSLA -> String
toHexAString =
    toRGBA >> RGBA.toHexAString
