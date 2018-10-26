module ListFilter exposing (Filter(..), Model, matchesOld)

import Todo exposing (TodoItem)


type Filter
    = Future
    | Active
    | Completed


type alias Model =
    { selected : Filter
    , modifiedAt : Int
    }


matchesOld : Int -> Filter -> TodoItem -> Bool
matchesOld referenceTime filter todo =
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


matches : TodoItem -> Model -> Bool
matches todo { selected, modifiedAt } =
    let
        completed =
            Todo.isCompleted todo

        inFuture =
            Todo.isScheduledAfter modifiedAt todo
    in
    case selected of
        Future ->
            not completed && inFuture

        Active ->
            not completed && not inFuture

        Completed ->
            completed
