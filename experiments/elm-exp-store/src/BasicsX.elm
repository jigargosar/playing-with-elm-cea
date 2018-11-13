module BasicsX exposing
    ( Encoder
    , Getter
    , Millis
    , Pred
    , PredList
    , activeElement
    , allPass
    , applyMaybe
    , applyMaybeFn2
    , applyTo
    , applyTo2
    , attemptFocus
    , attemptFocusMaybeDomId
    , defaultEmptyStringTo
    , eq0
    , eqs
    , everyXSeconds
    , flip
    , ifElse
    , isWhitespaceOrEmptyString
    , maybeBool
    , maybeWhen
    , notPred
    , nowMilli
    , optionalOr
    , propEq
    , replaceHead
    , safeModBy
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
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Log
import Task exposing (Task)
import Time


type alias Pred a =
    a -> Bool


type alias PredList a =
    List (Pred a)


allPass : PredList a -> Pred a
allPass plist a =
    List.all (applyTo a) plist


notPred : Pred a -> Pred a
notPred pred =
    pred >> not


type alias Getter big small =
    big -> small


propEq : Getter big small -> small -> Pred big
propEq getter small =
    getter >> eqs small


replaceHead newHead list =
    case list of
        head :: tail ->
            newHead :: tail

        [] ->
            []


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


activeElement : Decoder a -> Decoder a
activeElement decoder =
    D.at [ "target", "ownerDocument", "activeElement" ] decoder


attemptFocus onSuccess onError domId =
    Browser.Dom.focus domId
        |> Task.attempt
            (unpackResult
                (\_ -> onError [ "Focus Error: #", domId, " NotFound" ])
                (\_ -> onSuccess)
            )


attemptFocusMaybeDomId onSuccess onError =
    unwrapMaybe Cmd.none (attemptFocus onSuccess onError)


ter b t f =
    if b then
        t

    else
        f


applyTo a fn =
    fn a


applyTo2 : a -> b -> (a -> b -> c) -> c
applyTo2 a b fn =
    fn a b


applyMaybeFn2 : a -> b -> Maybe (a -> b -> c) -> Maybe c
applyMaybeFn2 a b =
    Maybe.map (applyTo2 a b)


applyMaybe a =
    Maybe.map (applyTo a)


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


maybeWhen pred answerFn =
    ifElse pred (answerFn >> Just) (always Nothing)


unwrapMaybe : b -> (a -> b) -> Maybe a -> b
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


isWhitespaceOrEmptyString =
    String.trim >> String.isEmpty


safeModBy total num =
    if total == 0 then
        0

    else
        modBy total num



---- CODECS ----


tsDecoder now =
    D.map (when eq0 now) D.int


optionalOr propName propDecoder defaultValue =
    D.oneOf [ D.field propName propDecoder, D.succeed defaultValue ]


type alias Encoder a =
    a -> Value
