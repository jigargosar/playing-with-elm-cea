module ListFilter exposing
    ( Filter(..)
    , Model
    , Msg
    , changeFilterMsg
    , filterTodoList
    , init
    , isSelected
    , subscriptions
    , update
    )

import BasicsX exposing (Millis, everyXSeconds)
import Todo
import TodoStore
import UpdateReturn exposing (..)


type Filter
    = Future
    | Active
    | Completed


type alias Model =
    { filter : Filter, lastTickedAt : Millis }


init : Millis -> Model
init now =
    { filter = Active, lastTickedAt = now }


filterTodoList : List TodoStore.Item -> Model -> List TodoStore.Item
filterTodoList todoList { filter, lastTickedAt } =
    let
        pred todo =
            let
                completed =
                    Todo.isCompleted todo

                inFuture =
                    Todo.isScheduledAfter lastTickedAt todo
            in
            case filter of
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
    .filter >> (==) filter


type Msg
    = NoOp
    | SetFilter Filter
    | SetLastTickedAt Millis


changeFilterMsg : Filter -> Msg
changeFilterMsg =
    SetFilter


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        SetFilter newFilter ->
            pure { model | filter = newFilter }

        SetLastTickedAt now ->
            pure { model | lastTickedAt = now }


subscriptions model =
    Sub.batch
        [ everyXSeconds 10 SetLastTickedAt
        ]
