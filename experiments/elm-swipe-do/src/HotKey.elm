module HotKey exposing (Event, decoder)

import Json.Decode as D
import Json.Encode as E


type alias Event =
    { key : String
    , shift : Bool
    }


decoder =
    D.map2 Event
        (D.field "key" D.string)
        (D.field "shift" D.bool)
