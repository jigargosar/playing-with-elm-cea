module ListFilter exposing
    ( Filter(..)
    , Model
    , Msg
    , changeFilterMsg
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
    }


init : Int -> Model
init now =
    { selected = Active }


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
    | SetFilter Filter


changeFilterMsg : Filter -> Msg
changeFilterMsg =
    SetFilter


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        SetFilter newFilter ->
            pure { model | selected = newFilter }
