module UI exposing (backdrop, boolHtml, maybeHtml, noHtml, sDiv, toolbar)

import BasicsX exposing (unwrapMaybe)
import FeatherIcons
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
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
