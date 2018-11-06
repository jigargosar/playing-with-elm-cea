module UI exposing
    ( Disabled
    , backdrop
    , boolHtml
    , fBtn
    , fBtnSA
    , flexV
    , link
    , maybeHtml
    , root
    , row
    , rowS3
    , sClass
    , spacer
    , toolbar
    , txt
    , txtA
    , txtC
    )

import BasicsX exposing (unwrapMaybe)
import FeatherIcons
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Svg.Attributes


link url lbl =
    a [ href url ] [ text lbl ]


rowS3 classes attrs c =
    div (class ("flex flex-row hs3 items-center " ++ classes) :: attrs) c


row =
    rowS3


spacer =
    div [ class "flex-auto" ] []


fBtn : FeatherIcons.Icon -> msg -> Html msg
fBtn fIcon msg =
    button
        [ onClick msg
        , class "flex items-center justify-center pa0 ma0"
        ]
        [ fIcon |> FeatherIcons.toHtml [] |> Html.fromUnstyled ]


sClass =
    Svg.Attributes.class


fBtnSA svgAttrs fIcon msg =
    button
        [ onClick msg
        , class "flex items-center justify-center pa0 ma0"
        ]
        [ fIcon |> FeatherIcons.toHtml svgAttrs |> Html.fromUnstyled ]


type alias Disabled =
    Bool


noHtml =
    text ""


boolHtml bool html_ =
    if bool then
        html_

    else
        noHtml


maybeHtml htmlFn =
    unwrapMaybe noHtml htmlFn


txt l =
    div [] [ text l ]


txtC c l =
    div [ class c ] [ text l ]


txtA attrs l =
    div attrs [ text l ]


root =
    div [ class "flex flex-column w-100" ]


toolbar kids =
    div [ class "flex w-100 justify-center bg-black white" ]
        [ div [ class "flex w-100 measure-wide items-center" ] kids
        ]


flexV attrs =
    div (class "flex flex-column" :: attrs)


backdrop attrs =
    div (class "z-2 absolute absolute--fill bg-black-40 flex items-center justify-center" :: attrs)
