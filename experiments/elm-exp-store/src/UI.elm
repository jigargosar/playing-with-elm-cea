module UI exposing (appBar, backdrop, boolHtml, maybeHtml, noHtml, sDiv, section1, toolbar)

import BasicsX exposing (unwrapMaybe)
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


backdrop attrs =
    div (class "z-2 absolute absolute--fill bg-black-40 flex items-center justify-center" :: attrs)


appBar =
    sDiv [ rowCXY, w100, bg "black", fg "white", elevation 3 ]


section1 =
    sDiv [ rowCY, w100, maxWidth (px 1024) ]
