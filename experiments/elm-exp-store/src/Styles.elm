module Styles exposing (flexAuto, flexRow, hs, pointer, vs, w100P)

import Css exposing (pct, rem, zero)


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
