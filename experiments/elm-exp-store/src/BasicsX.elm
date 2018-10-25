module BasicsX exposing
    ( Encoder
    , eq0
    , eqs
    , flip
    , ifElse
    , maybeBool
    , optionalOr
    , ter
    , tsDecoder
    , unless
    , unpackResult
    , unwrapDecodeResult
    , unwrapMaybe
    , when
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Log


ter b t f =
    if b then
        t

    else
        f


ifElse b t f v =
    ter (b v) (t v) (f v)


when b t v =
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


unwrapDecodeResult : (Log.Messages -> c) -> (a -> c) -> Result D.Error a -> c
unwrapDecodeResult errFn okFn result =
    case result of
        Ok answer ->
            okFn answer

        Err error ->
            errFn (error |> D.errorToString >> List.singleton)


flip fn a b =
    fn b a



---- CODECS ----


tsDecoder now =
    D.map (when eq0 now) D.int


optionalOr propName propDecoder defaultValue =
    D.oneOf [ D.field propName propDecoder, D.succeed defaultValue ]


type alias Encoder a =
    a -> Value
