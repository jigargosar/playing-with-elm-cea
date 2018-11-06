module UI.Btn exposing (btn, icon, iconButton)

import Css
import FeatherIcons
import Html.Styled exposing (button, fromUnstyled, styled)
import Html.Styled.Events exposing (onClick)
import Styles exposing (..)


icon i msg =
    styled button
        [ btnReset
        , fZero
        ]
        [ onClick msg ]
        [ i |> FeatherIcons.toHtml [] >> fromUnstyled ]


btn =
    styled button
        [ btnReset
        , pRm 0.5
        , tl
        , flexAuto
        , Css.fontSize (rem 0.8)
        , Css.property "color" "gray"
        , Css.hover
            [ Css.property "color" "red"
            ]
        ]


iconButton =
    styled button
        [ btnReset
        , pRm 0.5
        , flexAuto
        , Css.fontSize (rem 0.8)
        , Css.property "color" "gray"
        , Css.hover
            [ Css.property "color" "red"
            ]
        ]
