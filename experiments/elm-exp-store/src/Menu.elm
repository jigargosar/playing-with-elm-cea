module Menu exposing (render)

import Css exposing (..)
import DomEvents exposing (..)
import Html.Styled exposing (Attribute, Html, div, styled)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Keyed exposing (node)
import Styles exposing (..)


type alias ViewConfig child msg =
    { domId : DomId
    , children : List child
    , containerStyles : List Css.Style
    , childContent : child -> List (Html msg)
    }


render : ViewConfig child msg -> Html msg
render config =
    styled div
        ([ bg "white"
         , elevation 4
         , borderRadius (rem 0.5)
         ]
            ++ config.containerStyles
        )
        [ id config.domId ]
        (config.children
            |> List.map (config.childContent >> div [])
        )
