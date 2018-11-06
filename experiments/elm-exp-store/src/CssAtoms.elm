module CssAtoms exposing
    ( aic
    , b0
    , btnReset
    , dFlex
    , fDCol
    , fDRow
    , fa
    , fz0
    , jcc
    , lh0
    , m0
    , p0
    , ptr
    , tc
    , tl
    , ttu
    , w100
    )

import Css exposing (..)


p0 =
    padding zero


m0 =
    margin zero


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


fDCol =
    flexDirection column


fDRow =
    flexDirection row


aic =
    alignItems center


jcc =
    justifyContent center


fa =
    flex auto


ptr =
    cursor pointer


btnReset =
    batch
        [ p0
        , m0
        , tl
        , property "-webkit-appearance" "none"
        , backgroundColor transparent
        , b0
        , focus
            [ outlineWidth (px 2)
            , outlineOffset (px 2)
            ]
        ]


lh0 =
    lineHeight zero


fz0 =
    fontSize (px 0)
