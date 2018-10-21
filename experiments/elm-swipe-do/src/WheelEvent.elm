module WheelEvent exposing (WheelEvent, decoder)

import Json.Decode as D
import Json.Encode as E


type alias WheelEvent =
    { deltaX : Float
    , deltaY : Float
    }


decoder : D.Decoder WheelEvent
decoder =
    D.map2 WheelEvent
        (D.field "deltaX" D.float)
        (D.field "deltaY" D.float)
