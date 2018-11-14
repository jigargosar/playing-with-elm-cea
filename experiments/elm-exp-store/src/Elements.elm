module Elements exposing (highlightedChars, list, listItem, tag, viewBackdrop, wrapperLayout)

import Element exposing (Element, centerX, centerY, column, el, fill, height, rgb, rgb255, rgba, row, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


tagColor =
    rgb255 121 184 255


selectedColor =
    rgb255 205 236 255


backdropColor =
    rgba 0 0 0 0.2


spacing1 =
    Element.spacing 4


padding1 =
    Element.padding 4


spacing2 =
    Element.spacing 8


pa2 =
    Element.padding 8


wrapperLayout =
    Element.layout [ rootFontFamily, rootFontSize ]


tag : String -> Element msg
tag content =
    el
        [ Font.size 10
        , Background.color tagColor
        , Border.rounded 4
        , padding1
        ]
        (Element.text content)


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


rootFontSize =
    Font.size 16


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
            ++ [ spacing2, width fill, pa2 ]
            ++ attrs
        )


list =
    column [ width fill ]


viewBackdrop attrs =
    row
        ([ width fill
         , height fill
         , Background.color backdropColor
         , Font.color (rgb 1 1 1)
         ]
            ++ attrs
        )
        [ Element.text "" ]
