module DomX exposing (DomId, WindowSize, domIdDecoder, onClickTargetId, onClickTargetIdHtml, onFocusIn, onFocusInHtml, onFocusOut, onFocusOutHtml)

import Html exposing (Attribute)
import Html.Events
import Html.Styled.Attributes
import Json.Decode as D exposing (Decoder)
import Task exposing (Task)


type alias WindowSize =
    { width : Int, height : Int }


type alias DomId =
    String


onFocusOutHtml msg =
    Html.Events.on "focusout" (D.succeed msg)


onFocusInHtml msg =
    Html.Events.on "focusin" (D.succeed msg)


onFocusIn =
    onFocusInHtml >> Html.Styled.Attributes.fromUnstyled


onFocusOut =
    onFocusOutHtml >> Html.Styled.Attributes.fromUnstyled


onClickTargetIdHtml : (DomId -> msg) -> Attribute msg
onClickTargetIdHtml toMsg =
    let
        targetIdDecoder : Decoder DomId
        targetIdDecoder =
            D.at [ "target", "id" ] D.string
    in
    Html.Events.on "click" <| D.map toMsg targetIdDecoder


onClickTargetId =
    onClickTargetIdHtml >> Html.Styled.Attributes.fromUnstyled


domIdDecoder : Decoder DomId
domIdDecoder =
    D.at [ "id" ] D.string
