module ViewSvgHelpers exposing (square, square2, view)

import ISvg exposing (iHeight, iTranslate, iWidth, iX, iY)
import Svg exposing (Svg, g, line, rect)
import Svg.Attributes as SA


view content =
    [ rect
        [ SA.width "100%"
        , SA.height "100%"
        , SA.fill "#fff"
        , SA.stroke "#cd37a9"
        , SA.strokeWidth "2"
        , SA.opacity "0.4"
        ]
        []
    , g [ SA.transform (iTranslate 0 0) ] [ content ]
    ]


square x y size fillS =
    rect
        [ iX x
        , iY y
        , iWidth size
        , iHeight size
        , SA.fill fillS
        ]
        []


square2 x y size spacing =
    let
        sizeWithOffset =
            size + spacing
    in
    rect
        [ iX (x * sizeWithOffset)
        , iY (y * sizeWithOffset)
        , iWidth size
        , iHeight size
        , SA.fill "#cd37a9"
        , SA.strokeWidth "2"
        , SA.stroke "#000"
        ]
        []
