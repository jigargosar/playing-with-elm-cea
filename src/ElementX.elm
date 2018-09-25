module ElementX exposing
    ( HSLA
    , RGBA
    , bc
    , black
    , fc
    , fz
    , grayscale
    , hsla
    , lightGray
    , p
    , pXY
    , scaled
    , scaledInt
    , toHSLA
    , white
    )

import Color
import Element
import Element.Background
import Element.Font


scaled : Int -> Float
scaled =
    Element.modular 16 1.25


scaledInt : Int -> Int
scaledInt =
    scaled >> round


fz : Int -> Element.Attr decorative msg
fz =
    scaledInt >> Element.Font.size


p : Int -> Element.Attribute msg
p =
    scaledInt >> Element.padding


pXY : Int -> Int -> Element.Attribute msg
pXY x y =
    Element.paddingXY (scaledInt x) (scaledInt y)


hsla : Float -> Float -> Float -> Float -> Element.Color
hsla h s l a =
    Color.hsla h s l a
        |> Color.toRgba
        |> (\{ red, green, blue, alpha } -> Element.rgba red green blue alpha)


type alias HSLA =
    { hue : Float, saturation : Float, lightness : Float, alpha : Float }


type alias RGBA =
    { red : Float, green : Float, blue : Float, alpha : Float }


toHSLA : Element.Color -> HSLA
toHSLA =
    Element.toRgb >> Color.fromRgba >> Color.toHsla


lightGray : Element.Color
lightGray =
    hsla 0 0 0.83 1


white : Element.Color
white =
    hsla 1 1 1 1


black : Element.Color
black =
    hsla 0 0 0 1


bc : Element.Color -> Element.Attr decorative msg
bc =
    Element.Background.color


fc : Element.Color -> Element.Attr decorative msg
fc =
    Element.Font.color


grayscale : Float -> Element.Color
grayscale l =
    hsla 0 0 l 1
