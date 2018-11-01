module UI exposing
    ( Disabled
    , backdrop
    , boolHtml
    , fBtn
    , fBtnSA
    , flexV
    , link
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

import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg.Attributes


link url lbl =
    a [ href url ] [ text lbl ]


rowS3 classes attrs =
    div (class ("flex flex-row hs3 items-center " ++ classes) :: attrs)


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
        [ fIcon |> FeatherIcons.toHtml [] ]


sClass =
    Svg.Attributes.class


fBtnSA svgAttrs fIcon msg =
    button
        [ onClick msg
        , class "flex items-center justify-center pa0 ma0"
        ]
        [ fIcon |> FeatherIcons.toHtml svgAttrs ]


type alias Disabled =
    Bool


boolHtml bool html_ =
    if bool then
        html_

    else
        text ""


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
