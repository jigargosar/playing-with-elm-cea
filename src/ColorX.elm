module ColorX exposing (RGBA, rgba)


type CH255
    = CH255 Int


type Alpha
    = Alpha Float


type RGBA
    = RGBA CH255 CH255 CH255 Alpha


intToCH255 : Int -> CH255
intToCH255 =
    clamp 0 255 >> CH255


floatToAlpha : Float -> Alpha
floatToAlpha =
    clamp 0 1 >> Alpha


rgba : Int -> Int -> Int -> Float -> RGBA
rgba r g b a =
    RGBA (intToCH255 r) (intToCH255 g) (intToCH255 b) (floatToAlpha a)


rgb : Int -> Int -> Int -> RGBA
rgb r g b =
    RGBA (intToCH255 r) (intToCH255 g) (intToCH255 b) (floatToAlpha 1)
