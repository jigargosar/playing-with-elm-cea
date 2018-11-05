module UI.Btn exposing (btn, icon)

import Css
import FeatherIcons
import Html.Styled exposing (button, fromUnstyled, styled)
import Html.Styled.Events exposing (onClick)
import Styles exposing (bw0, flexAuto, ma0, pa0, padding, px, rem, tl, zero)


resetButton =
    Css.batch
        [ pa0
        , ma0
        , Css.property "-webkit-appearance" "none"
        , Css.backgroundColor Css.transparent
        , bw0
        , Styles.pointer
        , Css.focus
            [ Css.outlineWidth (px 2)
            , Css.outlineOffset (px 2)
            ]
        ]


icon i msg =
    styled button
        [ resetButton
        , Css.lineHeight zero
        , Css.fontSize (px 0)
        ]
        [ onClick msg ]
        [ i |> FeatherIcons.toHtml [] >> fromUnstyled ]


btn =
    styled button
        [ resetButton
        , padding (rem 0.5)
        , tl
        , flexAuto
        , Css.fontSize (rem 0.8)
        , Css.property "color" "gray"
        , Css.hover
            [ Css.property "color" "red"
            ]
        ]
