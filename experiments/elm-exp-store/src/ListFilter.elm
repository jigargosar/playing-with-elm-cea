module ListFilter exposing
    ( Filter(..)
    , Model
    , Msg(..)
    , init
    , isSelected
    , matches
    , update
    )

import Todo exposing (TodoItem)
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


isSelected : Filter -> Model -> Bool
isSelected filter =
    .selected >> (==) filter


type Msg
    = NoOp
    | SwitchFilterTo Filter
    | SwitchFilterToWithNow Filter Int


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
