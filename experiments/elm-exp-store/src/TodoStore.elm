module TodoStore exposing (TodoStore)

import Json.Decode as D
import Json.Encode as E exposing (Value)
import Store exposing (Store)
import Todo exposing (Todo)


type alias TodoStore =
    Store Todo


load : Value -> Result D.Error TodoStore
load =
    Store.load Todo.decoder
