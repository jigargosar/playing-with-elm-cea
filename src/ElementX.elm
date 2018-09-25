module ElementX exposing
    ( bc
    , bcInherit
    , black
    , brc
    , clipFillWH
    , each
    , elevation
    , fc
    , fillWH
    , fromRGBA
    , fz
    , grayscale
    , hsla
    , inputNumber
    , labelNone
    , lightGray
    , maxRem
    , minRem
    , p
    , pEach
    , pXY
    , remToInt
    , scaled
    , scaledInt
    , scrollFillWH
    , sp
    , toHSLA
    , white
    )

import Color
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Hsla
import Html.Attributes
import Round


scaled : Int -> Float
scaled =
    Element.modular 16 1.25


scaledInt : Int -> Int
scaledInt =
    scaled >> round


remToInt : Float -> Int
remToInt remVal =
    remVal * 16 |> round


maxRem =
    remToInt >> Element.maximum


minRem =
    remToInt >> Element.minimum


sp =
    scaledInt >> Element.spacing


labelNone =
    Element.Input.labelLeft [] Element.none


fz : Int -> Element.Attr decorative msg
fz =
    scaledInt >> Element.Font.size


p : Int -> Element.Attribute msg
p =
    scaledInt >> Element.padding


pXY : Int -> Int -> Element.Attribute msg
pXY x y =
    Element.paddingXY (scaledInt x) (scaledInt y)


each =
    { bottom = 0, top = 0, left = 0, right = 0 }


pEach eachRec =
    Element.paddingEach eachRec


hsla : Float -> Float -> Float -> Float -> Element.Color
hsla h s l a =
    Color.hsla h s l a
        |> Color.toRgba
        |> (\{ red, green, blue, alpha } -> Element.rgba red green blue alpha)


toHSLA : Element.Color -> Hsla.HSLA
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


brc : Element.Color -> Element.Attr decorative msg
brc =
    Element.Border.color


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
                >> Maybe.withDefault config.min
                >> clamp config.min config.max
                >> config.onChange
        , label = config.label
        , text = config.value |> clamp config.min config.max |> Round.round config.round
        , placeholder = config.placeholder
        }


fromRGBA { red, green, blue, alpha } =
    Element.rgba red green blue alpha


fillWH : List (Element.Attribute msg)
fillWH =
    [ Element.height Element.fill
    , Element.width Element.fill
    ]


clipFillWH : List (Element.Attribute msg)
clipFillWH =
    fillWH ++ [ Element.clip ]


scrollFillWH : List (Element.Attribute msg)
scrollFillWH =
    fillWH ++ [ Element.scrollbars ]



--elevation1 : Element.Attr decorative msg
--elevation1 =
--    Element.Border.shadow
--        { offset = ( 2, 2 )
--        , size = 0
--        , blur = 4
--        , color = Element.rgba 0 0 0 0.4
--        }


elevation i =
    ("mdc-elevation--z" ++ String.fromInt i)
        |> Html.Attributes.class
        |> Element.htmlAttribute
