module WheelEvent exposing (WheelEvent)

import Json.Decode as D
import Json.Encode as E


type alias WheelEvent =
    { deltaX : Float
    , deltaY : Float
    }
