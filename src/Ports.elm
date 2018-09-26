port module Ports exposing (config, store)

import Json.Encode exposing (Value)


port config : Value -> Cmd msg


port store : ( String, Value ) -> Cmd msg
