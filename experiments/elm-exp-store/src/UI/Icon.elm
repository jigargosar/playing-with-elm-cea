module UI.Icon exposing (withDefault)

import FeatherIcons
import Html.Styled exposing (fromUnstyled)


withDefault =
    FeatherIcons.toHtml [] >> fromUnstyled
