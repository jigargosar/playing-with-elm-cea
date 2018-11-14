module TodoListSelection exposing
    ( Config
    , SelectedIndex
    , cycleSelectedIndexBy
    , getComputedSelectedIndex
    , getMaybeSelectedIndexOnFocusIn
    , getMaybeSelectedTodo
    )

import Array
import BasicsX exposing (eqs, safeModBy, unwrapMaybe)
import ContextStore exposing (ContextId)
import ContextTodoList
import DomX exposing (DomId)
import Focus exposing (FocusResult)
import Html.Styled exposing (Html)
import TodoStore exposing (Todo, TodoStore)


type alias SelectedIndex =
    Int


type alias Model =
    SelectedIndex


type alias Config =
    { todoStore : TodoStore
    , selectedContextId : ContextId
    , selectedIndex : Int
    }


getSelectedContextTodoList : Config -> List Todo
getSelectedContextTodoList { todoStore, selectedContextId } =
    todoStore |> TodoStore.listForContextId selectedContextId


getComputedSelectedIndex config =
    let
        ( active, completed ) =
            getSelectedContextTodoList config
                |> List.partition TodoStore.isNotDone

        total =
            List.length active
    in
    min (total - 1) config.selectedIndex


getMaybeSelectedTodo config =
    let
        ( active, completed ) =
            getSelectedContextTodoList config
                |> List.partition TodoStore.isNotDone

        total =
            List.length active
    in
    if total <= 0 then
        Nothing

    else
        active |> Array.fromList |> Array.get (getComputedSelectedIndex config)


cycleSelectedIndexBy : Int -> Config -> Maybe ( SelectedIndex, Todo )
cycleSelectedIndexBy num config =
    let
        ( active, completed ) =
            getSelectedContextTodoList config
                |> List.partition TodoStore.isNotDone

        total =
            List.length active
    in
    if total > 0 then
        let
            selectedIndex =
                safeModBy total (config.selectedIndex + num)
        in
        Array.fromList active
            |> Array.get selectedIndex
            |> Maybe.map (\todo -> ( selectedIndex, todo ))

    else
        Nothing


getMaybeSelectedIndexOnFocusIn todoId config =
    let
        ( active, completed ) =
            getSelectedContextTodoList config
                |> List.partition TodoStore.isNotDone
    in
    Array.fromList active
        |> Array.toIndexedList
        |> List.filter (Tuple.second >> .id >> eqs todoId)
        |> List.head
        |> Maybe.map Tuple.first


type Msg
    = Inc
    | Dec


type OutMsg
    = Focus DomId


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update config message model =
    case message of
        Inc ->
            ( model, Cmd.none )

        Dec ->
            ( model, Cmd.none )
