module Btn exposing (flat, flatPl0, flatPr0, icon, sIcon)

import Css exposing (hover)
import CssAtoms exposing (fgGray)
import FeatherIcons
import Html.Styled exposing (button, fromUnstyled, styled)
import Html.Styled.Events exposing (onClick)
import Icons
import Styles exposing (..)


icon =
    styled button
        [ btnReset
        , fZero
        , hover [ fg "red" ]
        ]


sIcon =
    styled icon


flat =
    styled button
        [ btnReset
        , p2Rm 0 0.5
        , fgGray
        , rowCY
        , hover [ fg "red" ]
        ]


flatPl0 =
    styled flat [ plRm 0 ]


flatPr0 =
    styled flat [ prRm 0 ]



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
