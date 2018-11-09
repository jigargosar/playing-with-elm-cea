module HtmlX exposing (when)

import BasicsX exposing (ifElse)
import Html.Styled exposing (Html)
import UI exposing (noHtml)


when : (a -> Bool) -> (a -> Html msg) -> a -> Html msg
when pred objToHtml =
    ifElse pred objToHtml (always noHtml)
