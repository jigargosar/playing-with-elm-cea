module Store.Item exposing (Item, Meta, Milli, decoder, encoder, init, new)

import BasicsX exposing (Encoder)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Task exposing (Task)
import Time


type alias Milli =
    Int


type alias Meta =
    { createdAt : Milli
    , modifiedAt : Milli
    , deleted : Bool
    }


metaDecoder : Decoder Meta
metaDecoder =
    D.map3 Meta
        (D.field "createdAt" D.int)
        (D.field "modifiedAt" D.int)
        (D.field "deleted" D.bool)


metaEncoder : Encoder Meta
metaEncoder model =
    E.object
        [ ( "createdAt", E.int model.createdAt )
        , ( "modifiedAt", E.int model.modifiedAt )
        , ( "deleted", E.bool model.deleted )
        ]


type alias Item attrs =
    { meta : Meta
    , attrs : attrs
    }


defaultMeta =
    { createdAt = 0, modifiedAt = 0, deleted = False }


init attrs =
    { meta = defaultMeta, attrs = attrs }


initWithNow : attrs -> Milli -> Item attrs
initWithNow attrs now =
    { meta = { defaultMeta | createdAt = now, modifiedAt = now }, attrs = attrs }


decoder : Decoder attrs -> Decoder (Item attrs)
decoder attrsDecoder =
    D.map2 Item
        (D.field "meta" metaDecoder)
        (D.field "attrs" attrsDecoder)


encoder : Encoder attrs -> Encoder (Item attrs)
encoder attrsEncoder model =
    E.object
        [ ( "meta", metaEncoder model.meta )
        , ( "attrs", attrsEncoder model.attrs )
        ]


new : attrs -> Task x (Item attrs)
new attrs =
    Time.now |> Task.map (Time.posixToMillis >> initWithNow attrs)
