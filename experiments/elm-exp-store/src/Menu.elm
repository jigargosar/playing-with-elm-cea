module Menu exposing (render)

import Html.Styled exposing (Html, div)
import Html.Styled.Keyed exposing (node)


type alias ViewConfig child msg =
    { children : List child
    , childContent : child -> List (Html msg)
    }


render : ViewConfig child msg -> Html msg
render config =
    div []
        (config.children
            |> List.map (config.childContent >> div [])
        )
