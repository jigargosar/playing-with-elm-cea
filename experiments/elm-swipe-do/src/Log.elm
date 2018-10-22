module Log exposing (Messages, Result, handle, warn)

import Port


type alias Messages =
    List String


type alias Result a =
    Result.Result Messages a


warn : String -> Messages -> Cmd msg
warn moduleName =
    (::) moduleName >> Port.warn


handle : a -> Result a -> ( a, Cmd msg )
handle default result =
    case result of
        Ok value ->
            ( value, Cmd.none )

        Err msgs ->
            ( default
            , warn "Log" msgs
            )
