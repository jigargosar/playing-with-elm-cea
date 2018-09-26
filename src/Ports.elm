port module Ports exposing (store)

import Json.Encode exposing (Value)


port store : ( String, Value ) -> Cmd msg
