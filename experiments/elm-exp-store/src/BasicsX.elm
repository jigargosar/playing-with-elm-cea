module BasicsX exposing
    ( DomId
    , Encoder
    , Millis
    , applyTo
    , attemptDomIdFocus
    , defaultEmptyStringTo
    , eq0
    , eqs
    , everyXSeconds
    , flip
    , ifElse
    , maybeBool
    , nowMilli
    , onClickTargetId
    , optionalOr
    , swap
    , ter
    , tsDecoder
    , unless
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


onClickTargetId : (DomId -> msg) -> Html.Attribute msg
onClickTargetId toMsg =
    let
        targetIdDecoder : Decoder DomId
        targetIdDecoder =
            D.at [ "target", "id" ] D.string
    in
    Html.Events.on "click" <| D.map toMsg targetIdDecoder


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



---- CODECS ----


tsDecoder now =
    D.map (when eq0 now) D.int


optionalOr propName propDecoder defaultValue =
    D.oneOf [ D.field propName propDecoder, D.succeed defaultValue ]


type alias Encoder a =
    a -> Value
