module ListFilter exposing (ListFilter(..), matches)

import Todo exposing (TodoItem)


type ListFilter
    = Future
    | Active
    | Completed


matches : Int -> ListFilter -> TodoItem -> Bool
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
