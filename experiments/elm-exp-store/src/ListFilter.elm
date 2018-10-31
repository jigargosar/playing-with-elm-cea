module ListFilter exposing
    ( Filter(..)
    , Model
    , Msg(..)
    , getFilteredLists
    , init
    , isSelected
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


getFilteredLists : Millis -> List TodoStore.Item -> Model -> List TodoStore.Item
getFilteredLists referenceTime todoList { selected } =
    let
        pred todo =
            let
                completed =
                    Todo.isCompleted todo

                inFuture =
                    Todo.isScheduledAfter referenceTime todo
            in
            case selected of
                Future ->
                    not completed && inFuture

                Active ->
                    not completed && not inFuture

                Completed ->
                    completed
    in
    List.filter pred todoList


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
