module Todo exposing (Todo, TodoCollection, TodoF, TodoList, addTodo, defaultTodoCollection, generator, updateTodo)

import Dict exposing (Dict)
import Generators exposing (boolGenerator, idGenerator, wordsGenerator)
import Random
import String.Extra


type alias Todo =
    { id : String, text : String, done : Bool }


type alias TodoF =
    Todo -> Todo


type alias TodoList =
    List Todo


type alias TodoCollection =
    Dict String Todo


type alias TodoCollectionF =
    TodoCollection -> TodoCollection


defaultTodoCollection : TodoCollection
defaultTodoCollection =
    let
        defaultTodoList : TodoList
        defaultTodoList =
            [ Todo "0" "Get Some Milk!" False, Todo "1" "Build Quick Prototype !!" False ]
    in
    defaultTodoList
        |> List.map (\t -> ( t.id, t ))
        |> Dict.fromList


generator : Random.Generator Todo
generator =
    Random.map3 (\id words done -> Todo id (words |> String.Extra.toTitleCase) done)
        idGenerator
        wordsGenerator
        boolGenerator


updateTodo : TodoF -> Todo -> TodoCollectionF
updateTodo fn todo =
    Dict.update todo.id (Maybe.map fn)


addTodo : Todo -> TodoCollectionF
addTodo todo =
    Dict.insert todo.id todo
