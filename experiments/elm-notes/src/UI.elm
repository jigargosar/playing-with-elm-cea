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


fBtn : FeatherIcons.Icon -> msg -> Html msg
fBtn icon =
    fDBtn icon False


type alias Disabled =
    Bool


fDBtn : FeatherIcons.Icon -> Disabled -> msg -> Html msg
fDBtn fIcon disabled_ msg =
    button [ disabled disabled_, onClick msg ] [ fIcon |> FeatherIcons.toHtml [] ]
