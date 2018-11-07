module UI exposing
    ( backdrop
    , fBtn
    , row
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


rowS3 classes attrs c =
    div (class ("flex flex-row hs3 items-center " ++ classes) :: attrs) c


row =
    rowS3


fBtn : FeatherIcons.Icon -> msg -> Html msg
fBtn fIcon msg =
    button
        [ onClick msg
        , class "flex items-center justify-center pa0 ma0"
        ]
        [ fIcon |> FeatherIcons.toHtml [] |> Html.fromUnstyled ]


txt l =
    div [] [ text l ]


txtC c l =
    div [ class c ] [ text l ]


txtA attrs l =
    div attrs [ text l ]


toolbar kids =
    div [ class "flex w-100 justify-center bg-black white" ]
        [ div [ class "flex w-100 measure-wide items-center" ] kids
        ]


backdrop attrs =
    div (class "z-2 absolute absolute--fill bg-black-40 flex items-center justify-center" :: attrs)
