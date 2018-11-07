module UI exposing (appBar, backdrop, boolHtml, maybeHtml, noHtml, sDiv, section1, toolbar)

import BasicsX exposing (..)
import Css exposing (..)
import CssAtoms exposing (..)
import FeatherIcons
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Styles exposing (..)
import Svg.Attributes


boolHtml bool html_ =
    if bool then
        html_

    else
        noHtml


maybeHtml htmlFn =
    unwrapMaybe noHtml htmlFn


noHtml =
    text ""


sDiv =
    styled div


toolbar kids =
    div [ class "flex w-100 justify-center bg-black white" ]
        [ div [ class "flex w-100 measure-wide items-center" ] kids
        ]


backdrop =
    sDiv [ position absolute, absFill, rowCXY, bcBlackA 0.4 ]


appBar =
    sDiv [ rowCXY, w100, bg "black", fg "white", elevation 3 ]


section1 =
    sDiv [ rowCY, w100, maxWidth (px 1024) ]
