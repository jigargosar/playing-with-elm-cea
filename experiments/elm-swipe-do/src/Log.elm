module Log exposing (Messages, Result, resultWithDefault, warn)

import Port


type alias Messages =
    List String


type alias Result a =
    Result.Result Messages a


warn : String -> Messages -> Cmd msg
warn moduleName =
    (::) moduleName >> Port.warn


resultWithDefault : a -> Result a -> ( a, Cmd msg )
resultWithDefault default result =
    case result of
        Ok value ->
            ( value, Cmd.none )

        Err msgs ->
            ( default
            , warn "Log" msgs
            )
