module Store exposing (Id, Item, Store, initEmpty, insert, load, toIdItemPairList)

import BasicsX exposing (Encoder)
import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder, decodeValue)
import Json.Encode as E exposing (Value)
import Random exposing (Generator, Seed)
import Store.Item exposing (Item)


type alias Id =
    String


type alias CacheName =
    String


type alias Dict_ attrs =
    Dict Id (Item attrs)


type alias Model attrs =
    { dict : Dict_ attrs }


initEmpty : Model attrs
initEmpty =
    init Dict.empty


init : Dict_ attrs -> Model attrs
init dict =
    { dict = dict }


decoder : Decoder attrs -> Decoder (Model attrs)
decoder attrsDecoder =
    D.map init
        (D.dict (Store.Item.decoder attrsDecoder))


encode : Encoder attrs -> Encoder (Model attrs)
encode attrsEncoder model =
    E.object
        [ ( "dict", E.dict identity (Store.Item.encoder attrsEncoder) model.dict )
        ]


load : Decoder attrs -> Value -> Result D.Error (Model attrs)
load =
    decoder >> decodeValue


insert : ( Id, Item attrs ) -> Model attrs -> Model attrs
insert ( id, item ) model =
    { model | dict = Dict.insert id item model.dict }


toIdItemPairList : Model attrs -> List ( Id, Item attrs )
toIdItemPairList =
    .dict >> Dict.toList



---- External


type alias Store item =
    Model item


type alias Item attrs =
    Store.Item.Item attrs
