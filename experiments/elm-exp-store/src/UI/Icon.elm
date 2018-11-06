module UI.Icon exposing (withDefault)

import FeatherIcons exposing (toHtml)
import Html.Styled exposing (fromUnstyled)


withDefault =
    FeatherIcons.withStrokeWidth 0.01 >> toHtml [] >> fromUnstyled
