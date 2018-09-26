module SvgView exposing (view)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


roundedRect : Html msg
roundedRect =
    Svg.rect
        [ x "100"
        , y "10"
        , width "100"
        , height "100"
        , rx "15"
        , ry "15"
        , color "red"
        , fill "#caf3f5"
        ]
        []


svgView : Html msg
svgView =
    Svg.svg
        [ width "100%"
        , viewBox "0 0 500 100"
        ]
        [ Svg.rect [ width "100%", height "100%", fill "#361110" ] []
        , roundedRect
        ]


view : Html msg
view =
    svg
        [ width "120"
        , height "120"
        , width "100%"
        , viewBox "0 0 120 120"
        ]
        [ rect
            [ x "10"
            , y "10"
            , width "100"
            , height "100"
            , rx "15"
            , ry "15"
            ]
            []
        , circle
            [ cx "50"
            , cy "50"
            , r "50"
            ]
            []
        , svgView
        ]
