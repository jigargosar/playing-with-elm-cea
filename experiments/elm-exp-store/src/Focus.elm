module Focus exposing (FocusResult, FocusTask, attempt, attemptMaybe, task)

import Browser.Dom
import DomX exposing (DomId)
import Task exposing (Task)


type alias FocusTask =
    Task DomId DomId


type alias FocusResult =
    Result DomId DomId


task : DomId -> FocusTask
task domId =
    Browser.Dom.focus domId
        |> Task.mapError (always domId)
        |> Task.map (always domId)


attempt : (FocusResult -> msg) -> DomId -> Cmd msg
attempt resultToMsg domId =
    Task.attempt resultToMsg <| task domId


attemptMaybe : (FocusResult -> msg) -> Maybe DomId -> Cmd msg
attemptMaybe resultToMsg maybeDomId =
    case maybeDomId of
        Nothing ->
            Cmd.none

        Just domId ->
            attempt resultToMsg domId
