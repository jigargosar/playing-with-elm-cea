module Styles exposing (absFill, aic, aife, ambient, ambientColor, b0, bc, bcBlackA, bg, blackA, boolCss, btnReset, btnResetStyles, centerCenter, dFlex, dFlexCol, dFlexRow, elevation, fBody, fDCol, fDRow, fDir, fZero, fa, fg, fgGray, fwb, fwl, fz, fz0, fzPx, hs, jcc, lh, lh0, lhNum, m0, mRm, mbRm, noStyle, p0, p2Rm, pColor, pRm, penumbra, penumbraColor, pl0, plRm, prRm, ptr, rowBottomY, rowCXY, rowCY, spacing0, tc, tl, transitionFadeIn, ttl, ttu, umbra, umbraColor, vs, w100)

import Array
import BasicsX exposing (..)
import Css exposing (..)
import Css.Transitions as CT exposing (transition)
import Html.Styled exposing (div, styled, text)


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


ttl =
    textTransform lowercase


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


pRm value =
    padding (rem value)


p2Rm v1 v2 =
    padding2 (rem v1) (rem v2)


plRm =
    paddingLeft << rem


prRm =
    paddingRight << rem


mRm =
    margin << rem


mbRm =
    marginBottom << rem


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
        [ m0
        , p0
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


btnResetStyles =
    [ m0
    , p0
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
    lineHeight (rem 1.5)


fBody =
    batch [ fz, lh ]


fZero =
    batch [ fz0, lh0 ]


bg =
    property "background-color"


fg =
    property "color"


bc =
    backgroundColor


absFill =
    [ top, bottom, left, right ] |> List.map (applyTo zero) |> batch


bcBlackA =
    bc << blackA


blackA =
    rgba 0 0 0


boolCss bool t =
    if bool then
        batch t

    else
        noStyle


noStyle =
    batch []


fwb =
    fontWeight bold


fwl =
    fontWeight lighter


umbraColor =
    "rgba(0, 0, 0, 0.2)"


penumbraColor =
    "rgba(0, 0, 0, 0.14)"


ambientColor =
    "rgba(0, 0, 0, 0.12)"


umbra =
    Array.fromList
        [ "0px 0px 0px 0px"
        , "0px 2px 1px -1px"
        , "0px 3px 1px -2px"
        , "0px 3px 3px -2px"
        , "0px 2px 4px -1px"
        , "0px 3px 5px -1px"
        , "0px 3px 5px -1px"
        , "0px 4px 5px -2px"
        , "0px 5px 5px -3px"
        , "0px 5px 6px -3px"
        , "0px 6px 6px -3px"
        , "0px 6px 7px -4px"
        , "0px 7px 8px -4px"
        , "0px 7px 8px -4px"
        , "0px 7px 9px -4px"
        , "0px 8px 9px -5px"
        , "0px 8px 10px -5px"
        , "0px 8px 11px -5px"
        , "0px 9px 11px -5px"
        , "0px 9px 12px -6px"
        , "0px 10px 13px -6px"
        , "0px 10px 13px -6px"
        , "0px 10px 14px -6px"
        , "0px 11px 14px -7px"
        , "0px 11px 15px -7px"
        ]


penumbra =
    Array.fromList
        [ "0px 0px 0px 0px"
        , "0px 1px 1px 0px"
        , "0px 2px 2px 0px"
        , "0px 3px 4px 0px"
        , "0px 4px 5px 0px"
        , "0px 5px 8px 0px"
        , "0px 6px 10px 0px"
        , "0px 7px 10px 1px"
        , "0px 8px 10px 1px"
        , "0px 9px 12px 1px"
        , "0px 10px 14px 1px"
        , "0px 11px 15px 1px"
        , "0px 12px 17px 2px"
        , "0px 13px 19px 2px"
        , "0px 14px 21px 2px"
        , "0px 15px 22px 2px"
        , "0px 16px 24px 2px"
        , "0px 17px 26px 2px"
        , "0px 18px 28px 2px"
        , "0px 19px 29px 2px"
        , "0px 20px 31px 3px"
        , "0px 21px 33px 3px"
        , "0px 22px 35px 3px"
        , "0px 23px 36px 3px"
        , "0px 24px 38px 3px"
        ]


ambient =
    Array.fromList
        [ "0px 0px 0px 0px"
        , "0px 1px 3px 0px"
        , "0px 1px 5px 0px"
        , "0px 1px 8px 0px"
        , "0px 1px 10px 0px"
        , "0px 1px 14px 0px"
        , "0px 1px 18px 0px"
        , "0px 2px 16px 1px"
        , "0px 3px 14px 2px"
        , "0px 3px 16px 2px"
        , "0px 4px 18px 3px"
        , "0px 4px 20px 3px"
        , "0px 5px 22px 4px"
        , "0px 5px 24px 4px"
        , "0px 5px 26px 4px"
        , "0px 6px 28px 5px"
        , "0px 6px 30px 5px"
        , "0px 6px 32px 5px"
        , "0px 7px 34px 6px"
        , "0px 7px 36px 6px"
        , "0px 8px 38px 7px"
        , "0px 8px 40px 7px"
        , "0px 8px 42px 7px"
        , "0px 9px 44px 8px"
        , "0px 9px 46px 8px"
        ]


elevation z =
    property "box-shadow"
        ([ umbra |> Array.get z |> Maybe.withDefault ""
         , umbraColor
         , ","
         , penumbra |> Array.get z |> Maybe.withDefault ""
         , penumbraColor
         , ","
         , ambient |> Array.get z |> Maybe.withDefault ""
         , ambientColor
         ]
            |> String.join " "
        )


transitionFadeIn =
    transition
        [ CT.opacity3 150 1 CT.easeIn

        --    ,CT.opacity3 1000 1 CT.easeIn
        ]
