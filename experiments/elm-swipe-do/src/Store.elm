module Store exposing (Id, Store, insert)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import Random exposing (Generator, Seed)


type alias Id =
    String


type alias Model item =
    { dict : Dict Id item }


init : Dict Id item -> Model item
init dict =
    { dict = dict }


decoder : Decoder item -> Decoder (Model item)
decoder itemDecoder =
    D.map init (D.dict itemDecoder)


type alias Encoder a =
    a -> Value


encode : Encoder item -> Model item -> Value
encode itemEncoder model =
    E.object
        [ ( "dict", E.dict identity itemEncoder model.dict )
        ]


insert : ( Id, item ) -> Model item -> Model item
insert ( id, item ) model =
    { model | dict = Dict.insert id item model.dict }



---- External


type alias Store item =
    Model item
