module BasicsX exposing
    ( eq0
    , eqs
    , flip
    , ifElse
    , maybeBool
    , optionalOr
    , recoverErr
    , ter
    , tsDecoder
    , unless
    , unwrapMaybe
    , when
    )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E


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


recoverErr fn result =
    case result of
        Ok answer ->
            answer

        Err error ->
            fn error



---- CODECS ----


tsDecoder now =
    D.map (when eq0 now) D.int


optionalOr propName propDecoder defaultValue =
    D.oneOf [ D.field propName propDecoder, D.succeed defaultValue ]


flip fn a b =
    fn b a
