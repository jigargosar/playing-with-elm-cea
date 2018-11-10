module HtmlX exposing (emptyWhen, keyedDiv, when)

import BasicsX exposing (ifElse)
import Html.Styled exposing (Html)
import Html.Styled.Keyed
import UI exposing (noHtml)


when : (a -> Bool) -> (a -> Html msg) -> a -> Html msg
when pred objToHtml =
    ifElse pred objToHtml (always noHtml)


emptyWhen : (a -> Bool) -> (a -> List (Html msg)) -> a -> List (Html msg)
emptyWhen pred objToElementList =
    ifElse pred objToElementList (always [])


keyedDiv =
    Html.Styled.Keyed.node "div"
