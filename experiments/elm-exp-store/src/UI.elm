module UI exposing (appBar, backdrop, boolHtml, fromElement, maybeHtml, noHtml, row, sDiv, section1)

import BasicsX exposing (..)
import Css exposing (..)
import Element
import Element.Font
import Elements
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


backdrop =
    sDiv [ position absolute, absFill, rowCXY, bcBlackA 0.4 ]


appBar =
    sDiv [ flexShrink (int 0), rowCXY, w100, bg "black", fg "white", elevation 3 ]


section1 =
    sDiv [ rowCY, w100, maxWidth (px 940) ]


type alias HtmlNode msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


type ContainerOption
    = CX
    | CY


rowContainerOptionToStyle opt =
    case opt of
        CX ->
            justifyContent center

        CY ->
            alignItems center


rowStyles containerOptions =
    [ dFlexRow, fa ] ++ List.map rowContainerOptionToStyle containerOptions


row : List ContainerOption -> HtmlNode msg
row containerOptions =
    styled div (rowStyles containerOptions)


fromElement el =
    sDiv [] [] [ Html.fromUnstyled <| Element.layout [ Elements.rootFontFamily ] el ]
