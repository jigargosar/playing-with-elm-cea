module Elements exposing (myElement, rootFontFamily, tag)

import Element exposing (Element, centerY, el, fill, height, padding, paddingXY, rgb, rgb255, row, spacing)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


myElement : Element msg
myElement =
    el
        [ Background.color (rgb 0 0.5 0)
        , Border.color (rgb 0 0.7 0)
        ]
        (Element.text "You've made a stylish element!")


tagColor =
    rgb255 121 184 255


tag : String -> Element msg
tag content =
    row
        [ centerY
        , height fill
        , Background.color tagColor
        , Border.rounded 4
        , paddingXY 4 0
        ]
        [ el
            [ Font.size 10
            ]
            (Element.text content)
        ]


rootFontFamily =
    [ "-apple-system"
    , "BlinkMacSystemFont"
    , "avenir next"
    , "avenir"
    , "helvetica neue"
    , "helvetica"
    , "ubuntu"
    , "roboto"
    , "noto"
    , "segoe ui"
    , "arial"
    , "sans-serif"
    ]
        |> List.map Font.typeface
        |> Font.family
