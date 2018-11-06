module Styles exposing
    ( b0
    , btnReset
    , centerCenter
    , dFlexRow
    , flexAuto
    , hs
    , m0
    , margin
    , p0
    , padding
    , pct
    , pointer
    , pr
    , px
    , rem
    , rowBottomY
    , rowC
    , rowCY
    , spacing0
    , tl
    , ttu
    , vs
    , w100
    , zero
    )

import Css exposing (..)


padding =
    Css.padding


zero =
    Css.zero


margin =
    Css.margin


pct =
    Css.pct


rem =
    Css.rem


px =
    Css.px


p0 =
    padding zero


m0 =
    margin zero


spacing0 =
    batch [ p0, m0 ]


b0 =
    border zero


ttu =
    textTransform uppercase


tl =
    textAlign left


w100 =
    width (pct 100)


dFlexRow =
    batch [ displayFlex, flexDirection row ]


rowCY =
    batch [ dFlexRow, alignItems center ]


centerCenter =
    batch [ dFlexRow, alignItems center ]


rowC =
    batch [ dFlexRow, centerCenter ]


pr value =
    padding (rem value)


rowBottomY =
    batch [ dFlexRow, alignItems flexEnd ]


flexAuto =
    batch [ flex auto ]


vs =
    batch
        [ marginBottom (rem 0.5)
        , lastChild [ marginBottom zero ]
        ]


hs =
    batch
        [ marginRight (rem 0.5)
        , lastChild [ marginRight zero ]
        ]


pointer =
    cursor Css.pointer


btnReset =
    batch
        [ spacing0
        , tl
        , property "-webkit-appearance" "none"
        , backgroundColor transparent
        , b0
        , focus
            [ outlineWidth (px 2)
            , outlineOffset (px 2)
            ]
        ]
