module DbX exposing (..)

import Db exposing (Db)
import Dict
import Id
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


decoder : Decoder item -> Decoder (Db item)
decoder =
    D.dict >> D.map (Dict.toList >> List.map (Tuple.mapFirst Id.fromString) >> Db.fromList)


encode encodeItem db =
    db |> Db.toDict |> E.dict identity encodeItem
