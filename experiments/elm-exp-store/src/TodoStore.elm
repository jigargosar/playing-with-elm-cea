module TodoStore exposing (TodoStore, load)

import Json.Decode as D
import Json.Encode as E exposing (Value)
import Store exposing (Store)
import Todo exposing (Todo)


type alias TodoStore =
    Store Todo


storeName =
    "todoStore"


load : Value -> TodoStore
load =
    Store.load Todo.decoder
        >> Result.withDefault Store.initEmpty
