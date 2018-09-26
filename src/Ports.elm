port module Ports exposing (config)

import Json.Encode exposing (Value)


port config : Value -> Cmd msg
