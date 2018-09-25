module InternalColor exposing (HSLA, RGBA)


type alias HSLA =
    { hue : Float, saturation : Float, lightness : Float, alpha : Float }


type alias RGBA =
    { red : Float, green : Float, blue : Float, alpha : Float }
