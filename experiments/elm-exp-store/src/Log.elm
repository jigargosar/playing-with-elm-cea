module Log exposing (Line, Result, focusResult, resultWithDefault, warn)

import Focus exposing (FocusResult)
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


focusResult : String -> FocusResult -> Cmd msg
focusResult moduleName r =
    case r of
        Ok domId ->
            Cmd.none

        Err domId ->
            warn moduleName [ "Focus Failed For: ", domId ]


resultWithDefault : a -> Result a -> ( a, Cmd msg )
resultWithDefault default result =
    case result of
        Ok value ->
            ( value, Cmd.none )

        Err msgs ->
            ( default
            , warn "Log" msgs
            )
