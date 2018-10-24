module Store exposing (Id, Store, initEmpty, insert, load)

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
    { cacheName : CacheName, dict : Dict_ attrs }


initEmpty : CacheName -> Model attrs
initEmpty cacheName =
    init cacheName Dict.empty


init : CacheName -> Dict_ attrs -> Model attrs
init cacheName dict =
    { cacheName = cacheName, dict = dict }


decoder : Decoder attrs -> Decoder (Model attrs)
decoder attrsDecoder =
    D.map2 init
        (D.field "cacheName" D.string)
        (D.dict (Store.Item.decoder attrsDecoder))


encode : Encoder attrs -> Encoder (Model attrs)
encode attrsEncoder model =
    E.object
        [ ( "cacheName", E.string model.cacheName )
        , ( "dict", E.dict identity (Store.Item.encoder attrsEncoder) model.dict )
        ]


load : Decoder attrs -> Value -> Result D.Error (Model attrs)
load =
    decoder >> decodeValue


insert : ( Id, Item attrs ) -> Model attrs -> Model attrs
insert ( id, item ) model =
    { model | dict = Dict.insert id item model.dict }



---- External


type alias Store item =
    Model item
