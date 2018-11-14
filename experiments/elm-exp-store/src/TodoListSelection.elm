module TodoListSelection exposing (Model, SelectedIndex)

import ContextStore exposing (ContextId)
import TodoStore exposing (Todo, TodoStore)


type alias SelectedIndex =
    Int


type alias Model =
    SelectedIndex


type alias Config =
    { todoStore : TodoStore
    , selectedContextId : ContextId
    }


getSelectedContextTodoList : Config -> List Todo
getSelectedContextTodoList { todoStore, selectedContextId } =
    todoStore |> TodoStore.listForContextId selectedContextId
