module Menu exposing (render)

import Css exposing (..)
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
        [ position absolute ]
        []
        (config.children
            |> List.map (config.childContent >> div [])
        )
