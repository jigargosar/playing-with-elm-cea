module Elements exposing (highlightedChars, list, listItem, myElement, rootFontFamily, tag)

import Element exposing (Element, centerY, column, el, fill, height, padding, paddingXY, rgb, rgb255, row)
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


selectedColor =
    rgb255 205 236 255


spacing1 =
    Element.spacing 10


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


highlightedChars isHighlighted content =
    el [] (Element.text content)


boolAttrs bool attrs =
    [ if bool then
        attrs

      else
        []
    ]
        |> List.concat


listItem isSelected attrs =
    row
        (boolAttrs isSelected [ Background.color selectedColor ]
            ++ [ spacing1 ]
            ++ attrs
        )


list =
    column [ spacing1 ]
