module Log exposing (Line, Result, resultWithDefault, warn)

import Port


type alias Line =
    List String


type alias Result a =
    Result.Result Line a


warn : String -> Line -> Cmd msg
warn moduleName line =
    ([ moduleName, ": " ] ++ line |> String.join "")
        |> List.singleton
        |> Port.warn


resultWithDefault : a -> Result a -> ( a, Cmd msg )
resultWithDefault default result =
    case result of
        Ok value ->
            ( value, Cmd.none )

        Err msgs ->
            ( default
            , warn "Log" msgs
            )
