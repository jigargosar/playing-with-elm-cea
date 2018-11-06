module Styles exposing (b0, btnDefault, btnReset, centerCenter, dFlex, dFlexCol, dFlexRow, fBody, fDCol, fDRow, fDir, fZero, flexAuto, fz, fz0, fzPx, hs, lh, lh0, lhNum, m0, mRm, p0, pRm, pct, pointer, px, rem, rowBottomY, rowC, rowCY, spacing0, tl, ttu, vs, w100)

import Css exposing (..)


pct =
    Css.pct


rem =
    Css.rem


px =
    Css.px


p0 =
    padding zero


pRm value =
    padding (rem value)


mRm =
    margin << rem


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


tc =
    textAlign center


w100 =
    width (pct 100)


dFlex =
    displayFlex


fDir =
    flexDirection


fDCol =
    fDir column


fDRow =
    fDir row


dFlexRow =
    batch [ dFlex, fDRow ]


dFlexCol =
    batch [ dFlex, fDCol ]


rowCY =
    batch [ dFlexRow, alignItems center ]


centerCenter =
    batch [ dFlexRow, alignItems center ]


rowC =
    batch [ dFlexRow, centerCenter ]


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


fzPx =
    fontSize << px


lhNum =
    lineHeight << num


lh0 =
    lineHeight zero


fz0 =
    fzPx 0


fz =
    fzPx 16


lh =
    lhNum 1.5


fBody =
    batch [ fz, lh ]


fZero =
    batch [ fz0, lh0 ]


btnDefault =
    batch [ btnReset, pointer ]
