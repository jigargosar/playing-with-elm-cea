module Styles exposing
    ( bw0
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
    , w100P
    , zero
    )

import Css exposing (batch)


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


bw0 =
    Css.border zero


ttu =
    Css.textTransform Css.uppercase


tl =
    Css.textAlign Css.left


w100P =
    Css.width (pct 100)


dFlexRow =
    Css.batch [ Css.displayFlex, Css.flexDirection Css.row ]


rowCY =
    Css.batch [ dFlexRow, Css.alignItems Css.center ]


centerCenter =
    Css.batch [ dFlexRow, Css.alignItems Css.center ]


rowC =
    batch [ dFlexRow, centerCenter ]


pr value =
    padding (rem value)


rowBottomY =
    Css.batch [ dFlexRow, Css.alignItems Css.flexEnd ]


flexAuto =
    Css.batch [ Css.flex Css.auto ]


vs =
    Css.batch
        [ Css.marginBottom (rem 0.5)
        , Css.lastChild [ Css.marginBottom zero ]
        ]


hs =
    Css.batch
        [ Css.marginRight (rem 0.5)
        , Css.lastChild [ Css.marginRight zero ]
        ]


pointer =
    Css.cursor Css.pointer
