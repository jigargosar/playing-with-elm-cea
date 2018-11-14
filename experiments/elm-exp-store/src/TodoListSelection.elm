module TodoListSelection exposing
    ( Config
    , SelectedIndex
    , cycleSelectedIndexBy
    , getComputedSelectedIndex
    , getMaybeSelectedTodo
    )

import Array
import BasicsX exposing (eqs, safeModBy, unwrapMaybe)
import ContextStore exposing (ContextId)
import ContextTodoList
import Focus exposing (FocusResult)
import Html.Styled exposing (Html)
import TodoStore exposing (Todo, TodoStore)


type alias SelectedIndex =
    Int


type alias Config msg =
    { todoStore : TodoStore
    , selectedContextId : ContextId
    , selectedIndex : Int
    , onFocusResult : Focus.FocusResult -> msg
    }


getSelectedContextTodoList : Config msg -> List Todo
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


cycleSelectedIndexBy : Int -> Config msg -> Maybe ( SelectedIndex, Todo )
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


setSelectedIndexOnFocusIn todoId model =
    let
        ( active, completed ) =
            getSelectedContextTodoList model
                |> List.partition TodoStore.isNotDone

        selectedIndex =
            Array.fromList active
                |> Array.toIndexedList
                |> List.filter (Tuple.second >> .id >> eqs todoId)
                |> List.head
                |> unwrapMaybe model.selectedIndex Tuple.first
    in
    ( selectedIndex, Cmd.none )
