module UI.Btn exposing (btn, icon)

import Css
import FeatherIcons
import Html.Styled exposing (button, fromUnstyled, styled)
import Html.Styled.Events exposing (onClick)
import Styles exposing (bw0, ma0, pa0, px, zero)


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
        [ resetButton ]
