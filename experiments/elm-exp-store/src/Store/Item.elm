module Store.Item exposing (Item, Meta, Milli, itemDecoder, itemEncoder, newItemTask, setModifiedAt)

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



initItem : attrs -> Milli -> Item attrs
initItem attrs now =
    { meta = {  createdAt = now, modifiedAt = now , deleted = False}, attrs = attrs }


itemDecoder : Decoder attrs -> Decoder (Item attrs)
itemDecoder attrsDecoder =
    D.map2 Item
        (D.field "meta" metaDecoder)
        (D.field "attrs" attrsDecoder)


itemEncoder : Encoder attrs -> Encoder (Item attrs)
itemEncoder attrsEncoder model =
    E.object
        [ ( "meta", metaEncoder model.meta )
        , ( "attrs", attrsEncoder model.attrs )
        ]


newItemTask : attrs -> Task x (Item attrs)
newItemTask attrs =
    Time.now |> Task.map (Time.posixToMillis >> initItem attrs)


setModifiedAt model now =
    let
        meta =
            model.meta
    in
    { model | meta = { meta | modifiedAt = now } }
