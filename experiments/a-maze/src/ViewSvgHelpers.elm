module ViewSvgHelpers exposing (gridSquare, square, view)

import ISvg exposing (iHeight, iTranslate, iWidth, iX, iY)
import Svg exposing (Svg, defs, g, line, path, pattern, rect)
import Svg.Attributes as SA exposing (d, id)
import TypedSvg.Attributes exposing (patternUnits)
import TypedSvg.Types exposing (CoordinateSystem(..))


view content =
    [ defs []
        [ pattern
            [ id "tenthGrid"
            , iWidth 10
            , iHeight 10
            , patternUnits CoordinateSystemUserSpaceOnUse
            ]
            [ path
                [ d "M 10 0 L 0 0 0 10"
                , SA.fill "none"
                , SA.stroke "silver"
                , SA.strokeWidth "0.5"
                ]
                []
            ]
        , pattern
            [ id "grid"
            , iWidth 100
            , iHeight 100
            , patternUnits CoordinateSystemUserSpaceOnUse
            ]
            [ rect
                [ SA.width "100"
                , SA.height "100"
                , SA.fill "url(#tenthGrid)"
                ]
                []
            , path
                [ d "M 100 0 L 0 0 0 100"
                , SA.fill "none"
                , SA.stroke "gray"
                , SA.strokeWidth "1"
                ]
                []
            ]
        ]
    , rect
        [ SA.transform "scale(0.5,0.5)"
        , SA.width "200%"
        , SA.height "200%"
        , SA.fill "url(#grid)"
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


gridSquare x y size spacing =
    let
        sizeWithOffset =
            size + (spacing * 2)
    in
    rect
        [ iX (x * sizeWithOffset)
        , iY (y * sizeWithOffset)
        , iWidth size
        , iHeight size
        , SA.fill "#cd37a9"
        , SA.fill "none"
        , SA.strokeWidth "2"
        , SA.stroke "#000"
        ]
        []
