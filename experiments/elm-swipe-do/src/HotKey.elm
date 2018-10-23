module HotKey exposing (Event, decoder)

import Json.Decode as D
import Json.Encode as E


type alias Event =
    { key : String
    , shift : Bool
    , alt : Bool
    , ctrl : Bool
    , meta : Bool
    }


decoder =
    D.map5 Event
        (D.field "key" D.string)
        (D.field "shiftKey" D.bool)
        (D.field "altKey" D.bool)
        (D.field "ctrlKey" D.bool)
        (D.field "metaKey" D.bool)
