module ViewSvgHelpers exposing (view)

import ISvg exposing (iTranslate)
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
    , g [ SA.transform (iTranslate 20 20) ] [ content ]
    ]
