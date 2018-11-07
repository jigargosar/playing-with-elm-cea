module Menu exposing (render)

import Html.Styled exposing (Html, div, styled)
import Html.Styled.Keyed exposing (node)
import Styles exposing (..)


type alias ViewConfig child msg =
    { children : List child
    , childContent : child -> List (Html msg)
    }


render : ViewConfig child msg -> Html msg
render config =
    styled div
        []
        []
        (config.children
            |> List.map (config.childContent >> div [])
        )
