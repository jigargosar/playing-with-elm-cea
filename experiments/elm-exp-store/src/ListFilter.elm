module ListFilter exposing
    ( Filter(..)
    , Model
    , Msg(..)
    , init
    , isSelected
    , matchesSelectedIn
    , matchesSelectedWithReferenceTimeIn
    , update
    )

import BasicsX exposing (Millis)
import Todo
import TodoStore
import UpdateReturn exposing (..)


type Filter
    = Future
    | Active
    | Completed


type alias Model =
    { selected : Filter
    , modifiedAt : Int
    }


init : Int -> Model
init now =
    { selected = Active, modifiedAt = now }


getFilteredList : Millis -> List TodoStore.Item -> Model -> ( List TodoStore.Item, List TodoStore.Item )
getFilteredList referenceTime todoList model =
    ( List.filter (matchesSelectedIn model) todoList
    , List.filter (matchesSelectedWithReferenceTimeIn model referenceTime) todoList
    )


matchesSelectedIn : Model -> TodoStore.Item -> Bool
matchesSelectedIn { selected, modifiedAt } todo =
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


matchesSelectedWithReferenceTimeIn : Model -> Millis -> TodoStore.Item -> Bool
matchesSelectedWithReferenceTimeIn { selected, modifiedAt } referenceTime todo =
    let
        completed =
            Todo.isCompleted todo
    in
    case selected of
        Future ->
            False

        Active ->
            not completed
                && Todo.isScheduledAfter modifiedAt todo
                && Todo.isScheduledBefore referenceTime todo

        Completed ->
            False



--filter todoList model =
--    todoList |> List.filter matches


isSelected : Filter -> Model -> Bool
isSelected filter =
    .selected >> (==) filter


type Msg
    = NoOp
    | SwitchFilterTo Filter
    | SwitchFilterToWithNow Filter Int
    | UpdateModifiedAtToNow


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        SwitchFilterTo newFilter ->
            pure model
                |> performWithNow (SwitchFilterToWithNow newFilter)

        SwitchFilterToWithNow newFilter now ->
            pure { model | selected = newFilter, modifiedAt = now }

        UpdateModifiedAtToNow ->
            pure model
                |> performWithNow (SwitchFilterToWithNow model.selected)
