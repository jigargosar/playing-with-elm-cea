module ListFilter exposing (Filter(..), matches)

import Todo exposing (TodoItem)


type Filter
    = Future
    | Active
    | Completed


type alias Model =
    { filter : Filter
    , modifiedAt : Int
    }


matches : Int -> Filter -> TodoItem -> Bool
matches referenceTime filter todo =
    let
        completed =
            Todo.isCompleted todo

        inFuture =
            Todo.isScheduledAfter referenceTime todo
    in
    case filter of
        Future ->
            not completed && inFuture

        Active ->
            not completed && not inFuture

        Completed ->
            completed
