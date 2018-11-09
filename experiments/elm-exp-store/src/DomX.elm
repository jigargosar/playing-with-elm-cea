module DomX exposing (DomId, domIdDecoder, onClickTargetId, onFocusIn, onFocusOut)

import Browser.Dom
import Html.Styled exposing (Attribute)
import Html.Styled.Events as SE
import Json.Decode as D exposing (Decoder)
import Task exposing (Task)


type alias DomId =
    String


onFocusOut msg =
    SE.on "focusout" (D.succeed msg)


onFocusIn msg =
    SE.on "focusin" (D.succeed msg)


onClickTargetId : (DomId -> msg) -> Attribute msg
onClickTargetId toMsg =
    let
        targetIdDecoder : Decoder DomId
        targetIdDecoder =
            D.at [ "target", "id" ] D.string
    in
    SE.on "click" <| D.map toMsg targetIdDecoder


domIdDecoder : Decoder DomId
domIdDecoder =
    D.at [ "id" ] D.string
