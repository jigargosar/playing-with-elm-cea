module TodoStore exposing (TodoStore, load)

import Json.Decode as D
import Json.Encode as E exposing (Value)
import Store exposing (Store)
import Store.Item
import Todo exposing (Todo)


type alias TodoStore =
    Store Todo


storeName =
    "todoStore"


load : Value -> TodoStore
load =
    Store.load Todo.decoder
        >> Result.withDefault Store.initEmpty
        >> Store.insert ( "1", Todo.init "Foo Bar" |> Store.Item.init )
        >> Store.insert ( "2", Todo.init "Mr. Can Do" |> Store.Item.init )
