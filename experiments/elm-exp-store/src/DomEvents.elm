module DomEvents exposing (DomId, FocusResult, FocusTask, attemptFocus, attemptFocusMaybe, domIdDecoder, focus, onClickTargetId, onFocusIn, onFocusOut)

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


type alias FocusTask =
    Task DomId DomId


type alias FocusResult =
    Result DomId DomId


focus : DomId -> FocusTask
focus domId =
    Browser.Dom.focus domId
        |> Task.mapError (always domId)
        |> Task.map (always domId)


attemptFocus : (FocusResult -> msg) -> DomId -> Cmd msg
attemptFocus resultToMsg domId =
    Task.attempt resultToMsg <| focus domId


attemptFocusMaybe : (FocusResult -> msg) -> Maybe DomId -> Cmd msg
attemptFocusMaybe resultToMsg maybeDomId =
    case maybeDomId of
        Nothing ->
            Cmd.none

        Just domId ->
            attemptFocus resultToMsg domId
