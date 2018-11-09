module DomEvents exposing (DomId, attemptFocus, attemptFocusWithMaybeDomId, domIdDecoder, focus, onClickTargetId, onFocusIn, onFocusOut)

import Browser.Dom
import Html.Styled exposing (Attribute)
import Html.Styled.Events as SE
import Json.Decode as D exposing (Decoder)
import Task


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


focus domId =
    Browser.Dom.focus domId
        |> Task.mapError (always domId)
        |> Task.map (always domId)


attemptFocus resultToMsg domId =
    Task.attempt resultToMsg <| focus domId


attemptFocusWithMaybeDomId resultToMsg maybeDomId =
    case maybeDomId of
        Nothing ->
            Cmd.none

        Just domId ->
            attemptFocus resultToMsg domId
