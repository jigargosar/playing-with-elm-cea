module CssAtoms exposing
    ( aic
    , aife
    , b0
    , dFlex
    , fDCol
    , fDRow
    , fa
    , fgGray
    , fz0
    , jcc
    , lh0
    , m0
    , p0
    , pColor
    , pl0
    , ptr
    , tc
    , tl
    , ttu
    , w100
    )

import Css exposing (..)


p0 =
    padding zero


pl0 =
    paddingLeft zero


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


aife =
    alignItems flexEnd


fa =
    flex auto


ptr =
    cursor pointer


lh0 =
    lineHeight zero


fz0 =
    fontSize (px 0)


pColor =
    property "color"


fgGray =
    pColor "gray"
