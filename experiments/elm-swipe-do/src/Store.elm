module Store exposing (Id, Store)

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Random exposing (Generator, Seed)


type alias Id =
    String


idDecoder : Decoder Id
idDecoder =
    D.string


type alias Model item =
    { dict : Dict Id item }


init : Dict Id item -> Model item
init dict =
    { dict = dict }


decoder : Decoder item -> Decoder (Model item)
decoder =
    D.dict >> D.map init


insert : ( Id, item ) -> Model item -> Model item
insert ( id, item ) model =
    { model | dict = Dict.insert id item model.dict }



---- External


type alias Store item =
    Model item
