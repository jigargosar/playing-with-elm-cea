module Styles exposing
    ( bc
    , bg
    , boolCss
    , btnReset
    , centerCenter
    , dFlexCol
    , dFlexRow
    , fBody
    , fDir
    , fZero
    , fwb
    , fz
    , fzPx
    , hs
    , lh
    , lhNum
    , mRm
    , p2Rm
    , pRm
    , pct
    , plRm
    , px
    , rem
    , rowBottomY
    , rowCXY
    , rowCY
    , spacing0
    , vs
    )

import Css exposing (..)
import CssAtoms exposing (aic, b0, dFlex, fDCol, fDRow, fz0, jcc, lh0, m0, p0, ptr, tl)


pct =
    Css.pct


rem =
    Css.rem


px =
    Css.px


pRm value =
    padding (rem value)


p2Rm v1 v2 =
    padding2 (rem v1) (rem v2)


plRm =
    paddingLeft << rem


mRm =
    margin << rem


spacing0 =
    batch [ p0, m0 ]


fDir =
    flexDirection


dFlexRow =
    batch [ dFlex, fDRow ]


dFlexCol =
    batch [ dFlex, fDCol ]


rowCY =
    batch [ dFlexRow, aic ]


centerCenter =
    batch [ aic, jcc ]


rowCXY =
    batch [ dFlexRow, centerCenter ]


rowBottomY =
    batch [ dFlexRow, alignItems flexEnd ]


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


btnReset =
    batch
        [ spacing0
        , tl
        , property "-webkit-appearance" "none"
        , backgroundColor transparent
        , b0
        , ptr
        , fBody
        , focus
            [ outlineWidth (px 2)
            , outlineOffset (px 0)
            ]
        ]


fzPx =
    fontSize << px


lhNum =
    lineHeight << num


fz =
    fzPx 16


lh =
    lhNum 1.5


fBody =
    batch [ fz, lh ]


fZero =
    batch [ fz0, lh0 ]


bg =
    property "background-color"


bc =
    backgroundColor


boolCss bool t =
    if bool then
        Css.batch t

    else
        Css.batch []


fwb =
    fontWeight bold
