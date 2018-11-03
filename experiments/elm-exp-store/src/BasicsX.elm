module BasicsX exposing
    ( DomId
    , Encoder
    , Millis
    , activeElement
    , allPass
    , applyTo
    , attemptDomIdFocus
    , defaultEmptyStringTo
    , domIdDecoder
    , eq0
    , eqs
    , everyXSeconds
    , flip
    , ifElse
    , isWhitespaceOrEmptyString
    , maybeBool
    , nowMilli
    , onClickTargetId
    , onFocusIn
    , onFocusOut
    , optionalOr
    , swap
    , ter
    , tsDecoder
    , unless
    , unpackMaybe
    , unpackResult
    , unwrapDecodeResult
    , unwrapMaybe
    , when
    , withNowMilli
    )

import Browser.Dom
import Html exposing (Html)
import Html.Events
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Log
import Task exposing (Task)
import Time


type alias Millis =
    Int


nowMilli : Task x Millis
nowMilli =
    Task.map Time.posixToMillis Time.now


withNowMilli : (Millis -> msg) -> Cmd msg
withNowMilli toMsg =
    Task.perform toMsg nowMilli


everyXSeconds seconds toMsg =
    Time.every (1000 * seconds) (Time.posixToMillis >> toMsg)


type alias DomId =
    String


onFocusOut msg =
    Html.Events.on "focusout" (D.succeed msg)


onFocusIn msg =
    Html.Events.on "focusin" (D.succeed msg)


onClickTargetId : (DomId -> msg) -> Html.Attribute msg
onClickTargetId toMsg =
    let
        targetIdDecoder : Decoder DomId
        targetIdDecoder =
            D.at [ "target", "id" ] D.string
    in
    Html.Events.on "click" <| D.map toMsg targetIdDecoder


domIdDecoder : Decoder DomId
domIdDecoder =
    D.at [ "id" ] D.string


activeElement : Decoder a -> Decoder a
activeElement decoder =
    D.at [ "target", "ownerDocument", "activeElement" ] decoder


attemptDomIdFocus domId onSuccess onError =
    Browser.Dom.focus domId
        |> Task.attempt
            (unpackResult
                (\_ -> onError [ "Focus Error: #", domId, " NotFound" ])
                (\_ -> onSuccess)
            )


ter b t f =
    if b then
        t

    else
        f


applyTo a fn =
    fn a


ifElse b t f v =
    ter (b v) (t v) (f v)


defaultEmptyStringTo : String -> String -> String
defaultEmptyStringTo =
    when String.isEmpty << always


when : (a -> Bool) -> (a -> a) -> a -> a
when b t =
    ifElse b t identity


unless b =
    when (b >> not)


eqs =
    (==)


eq0 =
    eqs 0


maybeBool bool value =
    if bool then
        Just value

    else
        Nothing


unwrapMaybe dv fn =
    Maybe.map fn >> Maybe.withDefault dv


unpackMaybe dvFn fn mb =
    case mb of
        Nothing ->
            dvFn

        Just val ->
            fn val


unpackResult errFn okFn result =
    case result of
        Ok answer ->
            okFn answer

        Err error ->
            errFn error


unwrapDecodeResult : (Log.Line -> c) -> (a -> c) -> Result D.Error a -> c
unwrapDecodeResult errFn okFn result =
    case result of
        Ok answer ->
            okFn answer

        Err error ->
            errFn (error |> D.errorToString >> List.singleton)


flip fn a b =
    fn b a


swap ( a, b ) =
    ( b, a )


allPass : a -> List (a -> Bool) -> Bool
allPass item predList =
    List.all (\pred -> pred item) predList


isWhitespaceOrEmptyString =
    String.trim >> String.isEmpty



---- CODECS ----


tsDecoder now =
    D.map (when eq0 now) D.int


optionalOr propName propDecoder defaultValue =
    D.oneOf [ D.field propName propDecoder, D.succeed defaultValue ]


type alias Encoder a =
    a -> Value
