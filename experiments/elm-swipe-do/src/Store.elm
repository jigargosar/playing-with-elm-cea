module Store exposing (Id, Store, insert)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder, decodeValue)
import Json.Encode as E exposing (Value)
import Random exposing (Generator, Seed)


type alias Id =
    String


type alias CacheName =
    String


type alias Model item =
    { cacheName : CacheName, dict : Dict Id item }


init : CacheName -> Dict Id item -> Model item
init cacheName dict =
    { cacheName = cacheName, dict = dict }


decoder : Decoder item -> Decoder (Model item)
decoder itemDecoder =
    D.map2 init
        (D.field "cacheName" D.string)
        (D.dict itemDecoder)


encode : Encoder item -> Model item -> Value
encode itemEncoder model =
    E.object
        [ ( "cacheName", E.string model.cacheName )
        , ( "dict", E.dict identity itemEncoder model.dict )
        ]


load : Decoder item -> Value -> Result D.Error (Model item)
load =
    decoder >> decodeValue


insert : ( Id, item ) -> Model item -> Model item
insert ( id, item ) model =
    { model | dict = Dict.insert id item model.dict }



---- External


type alias Store item =
    Model item


type alias Encoder a =
    a -> Value
