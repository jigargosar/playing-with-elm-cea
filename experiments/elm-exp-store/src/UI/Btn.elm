module UI.Btn exposing (icon)

import Css
import FeatherIcons
import Html.Styled exposing (button, fromUnstyled, styled)
import Html.Styled.Events exposing (onClick)
import Styles exposing (bw0, ma0, pa0, px, zero)


icon i msg =
    styled button
        [ pa0
        , ma0
        , Css.property "-webkit-appearance" "none"
        , Css.backgroundColor Css.transparent
        , bw0
        , Styles.pointer
        , Css.lineHeight zero
        , Css.fontSize (px 0)
        ]
        [ onClick msg ]
        [ i |> FeatherIcons.toHtml [] >> fromUnstyled ]
