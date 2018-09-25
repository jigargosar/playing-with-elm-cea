module ElementX exposing
    ( HSLA
    , RGBA
    , bc
    , bcInherit
    , black
    , fc
    , fz
    , grayscale
    , hsla
    , inputNumber
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
import Element.Input
import Html.Attributes
import Round


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


bcInherit : Element.Attribute msg
bcInherit =
    Html.Attributes.style "background-color" "inherit" |> Element.htmlAttribute


fc : Element.Color -> Element.Attr decorative msg
fc =
    Element.Font.color


grayscale : Float -> Element.Color
grayscale l =
    hsla 0 0 l 1


inputNumber :
    List (Element.Attribute msg)
    ->
        { onChange : Float -> msg
        , step : Float
        , min : Float
        , max : Float
        , value : Float
        , round : Int
        , label : Element.Input.Label msg
        , placeholder : Maybe (Element.Input.Placeholder msg)
        }
    -> Element.Element msg
inputNumber attributes config =
    let
        typeNumber =
            Html.Attributes.type_ "number" |> Element.htmlAttribute

        step val =
            Html.Attributes.step (String.fromFloat val) |> Element.htmlAttribute

        min val =
            Html.Attributes.min (String.fromFloat val) |> Element.htmlAttribute

        max val =
            Html.Attributes.max (String.fromFloat val) |> Element.htmlAttribute

        emptyAttribute =
            Html.Attributes.attribute "" "" |> Element.htmlAttribute
    in
    Element.Input.text
        ([ bcInherit ]
            ++ attributes
            ++ [ typeNumber
               , step config.step
               , min config.min
               , max config.max
               ]
        )
        { onChange =
            String.toFloat
                >> Maybe.withDefault config.value
                >> config.onChange
        , label = config.label
        , text = config.value |> Round.round config.round
        , placeholder = config.placeholder
        }
