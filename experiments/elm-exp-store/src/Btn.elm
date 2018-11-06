module Btn exposing (flat, icon)

import Css exposing (hover)
import CssAtoms exposing (fgGray)
import FeatherIcons
import Html.Styled exposing (button, fromUnstyled, styled)
import Html.Styled.Events exposing (onClick)
import Styles exposing (..)


icon =
    styled button
        [ btnReset
        , fZero
        , hover [ fg "red" ]
        ]


flat =
    styled button
        [ btnReset
        , p2Rm 0 0.5
        , fgGray
        , rowCY
        , hover [ fg "red" ]
        ]



--flat =
--    styled button
--        [ btnReset
--        , pRm 0.5
--
--        --        , flexAuto
--        , Css.fontSize (rem 0.8)
--        , Css.property "color" "gray"
--        , Css.hover
--            [ Css.property "color" "red"
--            ]
--        ]
