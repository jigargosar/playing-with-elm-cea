module ColorX exposing (HSLA, RGBA, hsl, hsla, rgb, rgba)


type Alpha
    = Alpha Float


floatToAlpha : Float -> Alpha
floatToAlpha =
    clamp 0 1 >> Alpha


type CH255
    = CH255 Int


type alias RGBARecord =
    { r : CH255, g : CH255, b : CH255, a : Alpha }


type RGBA
    = RGBA RGBARecord


intToCH255 : Int -> CH255
intToCH255 =
    clamp 0 255 >> CH255


rgba : Int -> Int -> Int -> Float -> RGBA
rgba r g b a =
    RGBARecord (intToCH255 r) (intToCH255 g) (intToCH255 b) (floatToAlpha a)
        |> RGBA


rgb : Int -> Int -> Int -> RGBA
rgb r g b =
    rgba r g b 1


type HUE
    = HUE Int


type SL100
    = SL100 Int


type alias HSLARecord =
    { h : HUE, s : SL100, l : SL100, a : Alpha }


type HSLA
    = HSLA HSLARecord


intToHue : Int -> HUE
intToHue =
    clamp 0 360 >> HUE


intToSL100 : Int -> SL100
intToSL100 =
    clamp 0 360 >> SL100


hsla : Int -> Int -> Int -> Float -> HSLA
hsla h s l a =
    HSLARecord (intToHue h) (intToSL100 s) (intToSL100 l) (floatToAlpha a)
        |> HSLA


hsl : Int -> Int -> Int -> HSLA
hsl h s l =
    hsla h s l 1



--rgb : Int -> Int -> Int -> RGBA
--rgb r g b =
--    RGBARecord (intToCH255 r) (intToCH255 g) (intToCH255 b) (floatToAlpha 1)
--        |> RGBA
--hsla : Int -> Int -> Int -> Float -> RGBA
--hsla h s v a =
--    RGBARecord (intToCH255 r) (intToCH255 g) (intToCH255 b) (floatToAlpha a)
--        |> RGBA
--
