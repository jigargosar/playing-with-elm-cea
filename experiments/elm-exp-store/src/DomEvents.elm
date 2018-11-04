module DomEvents exposing (onFocusIn, onFocusOut)

import Html.Events
import Json.Decode as D
import Json.Encode as E


onFocusOut msg =
    Html.Events.on "focusout" (D.succeed msg)


onFocusIn msg =
    Html.Events.on "focusin" (D.succeed msg)
