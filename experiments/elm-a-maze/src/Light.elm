module Light exposing (..)

import Color exposing (Color)


type alias F a =
    a -> a


type alias ColorF =
    F Color


type alias CompactHsla =
    { h : Float, s : Float, l : Float, a : Float }


type alias CompactHslaF =
    F CompactHsla


toCompactHsla : Color -> CompactHsla
toCompactHsla =
    Color.toHsla >> \h -> { h = h.hue, s = h.saturation, l = h.lightness, a = h.alpha }


fromCompactHsla : CompactHsla -> Color
fromCompactHsla { h, s, l, a } =
    Color.hsla h s l a


overCompactHsla : CompactHslaF -> ColorF
overCompactHsla fn =
    toCompactHsla >> fn >> fromCompactHsla


saturate =
    Color.toHsla >> \h -> h
