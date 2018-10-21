module UI exposing (Disabled, boolHtml, fBtn, fDBtn, link, root, row, rowS3, spacer, toolbar, txt, txtC, txtCL)

import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


link url lbl =
    a [ href url ] [ text lbl ]


rowS3 classes attrs =
    div (class ("flex flex-row hs3 items-center " ++ classes) :: attrs)


row =
    rowS3


spacer =
    div [ class "flex-auto" ] []


fBtn : FeatherIcons.Icon -> msg -> Html msg
fBtn icon =
    fDBtn icon False


type alias Disabled =
    Bool


fDBtn : FeatherIcons.Icon -> Disabled -> msg -> Html msg
fDBtn fIcon disabled_ msg =
    button
        [ disabled disabled_
        , onClick msg
        , class "flex items-center justify-center pa0 ma0"
        ]
        [ fIcon |> FeatherIcons.toHtml [] ]


boolHtml bool html_ =
    if bool then
        html_

    else
        text ""


txt l =
    div [] [ text l ]


txtC c l =
    div [ class c ] [ text l ]


txtCL cl l =
    div [ classList cl ] [ text l ]


root =
    div [ class "flex flex-column w-100" ]


toolbar kids =
    div [ class "flex w-100 justify-center bg-black white" ]
        [ div [ class "flex w-100 measure-wide items-center" ] kids
        ]
