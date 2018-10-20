module UI exposing (link, row, spacer)

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


fBtn : msg -> FeatherIcons.Icon -> Html msg
fBtn msg fIcon =
    button [ onClick msg ] [ fIcon |> FeatherIcons.toHtml [] ]
