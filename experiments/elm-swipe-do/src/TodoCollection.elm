module TodoCollection exposing (TodoCollection, TodoIds)

import Collection exposing (Collection)
import Set exposing (Set)
import Todo exposing (Todo)


type alias TodoIds =
    Set Todo.Id


type alias TodoCollection =
    Collection Todo
