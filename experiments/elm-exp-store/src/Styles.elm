module Styles exposing
    ( bw0
    , flexAuto
    , flexRow
    , hs
    , ma0
    , margin
    , pa0
    , padding
    , pct
    , pointer
    , px
    , rem
    , ttu
    , vs
    , w100P
    , zero
    )

import Css


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


pa0 =
    padding zero


ma0 =
    margin zero


bw0 =
    Css.border zero


ttu =
    Css.textTransform Css.uppercase


w100P =
    Css.width (pct 100)


flexRow =
    Css.batch [ Css.displayFlex, Css.flexDirection Css.row ]


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
