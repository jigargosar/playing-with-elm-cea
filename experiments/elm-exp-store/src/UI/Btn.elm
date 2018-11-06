module UI.Btn exposing (flat, iconMsg)

import Css
import CssAtoms exposing (fgGray)
import FeatherIcons
import Html.Styled exposing (button, fromUnstyled, styled)
import Html.Styled.Events exposing (onClick)
import Styles exposing (..)


iconMsg i msg =
    styled button
        [ btnReset
        , fZero
        ]
        [ onClick msg ]
        [ i |> FeatherIcons.toHtml [] >> fromUnstyled ]


flat =
    styled button
        [ btnReset
        , p2Rm 0 0.5
        , fgGray
        , Css.hover
            [ Css.property "color" "red"
            ]
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
