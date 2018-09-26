module SvgView exposing (view)

import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


view : Html msg
view =
    svg
        [ width "100%"
        , viewBox "0 0 500 120"
        ]
        [ Svg.rect [ width "100%", height "100%", fill "#fff" ] []
        , rect
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
        , rect
            [ x "150"
            , y "10"
            , width "100"
            , height "100"
            , rx "15"
            , ry "15"
            , color "red"
            , fill "tomato"
            ]
            []
        ]
