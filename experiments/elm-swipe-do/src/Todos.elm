module Todos exposing (Msg(..), Todos, generator, update, view)

import Browser.Dom
import Collection exposing (Collection)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as D
import Json.Encode as E
import Port
import Random
import Task
import Todo exposing (Todo)
import UI exposing (flexV, row, txtC)


type Mode
    = List
    | Edit Todo.Id Todo.Content


type alias TodoCollection =
    Collection Todo


type alias Model =
    { mode : Mode, collection : TodoCollection }


type alias Todos =
    Model


setCollection : TodoCollection -> Model -> Model
setCollection collection model =
    { model | collection = collection }


setMode : Mode -> Model -> Model
setMode mode model =
    { model | mode = mode }


getTodoList =
    .collection >> Collection.items >> List.sortBy (.modifiedAt >> (*) -1)


generator : E.Value -> Random.Generator Model
generator enc =
    Collection.generator Todo.decoder enc
        |> Random.map (Model List)


type Msg
    = NoOp
    | NewClicked
    | Reset
    | NewAdded ( Todo, Collection Todo )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        NewAdded ( todo, collection ) ->
            ( setCollection collection model
                |> setMode (Edit todo.id (Todo.getContent todo))
            , Cmd.batch
                [ Port.focus "#todo-content-input"
                , Port.cacheTodoC (Collection.encode Todo.encode collection)
                ]
            )

        NewClicked ->
            ( model
            , Collection.createAndAdd Todo.init model.collection
                |> Task.perform NewAdded
            )

        Reset ->
            let
                newCollection =
                    Collection.reset model.collection
            in
            ( { model
                | collection = newCollection
              }
            , Port.cacheTodoC (Collection.encode Todo.encode newCollection)
            )


view : Model -> Html msg
view =
    viewList


viewList : Model -> Html msg
viewList model =
    model |> getTodoList >> List.map (viewTodo model.mode) >> div [ class "w-100 measure-narrow vs3" ]


viewTodo : Mode -> Todo -> Html msg
viewTodo mode todo =
    let
        defaultView =
            row "" [] [ viewTodoContent (Todo.getContent todo) ]
    in
    case mode of
        List ->
            defaultView

        Edit id content ->
            if todo.id == id then
                flexV [] [ input [ Html.Attributes.id "todo-content-input", value content ] [] ]

            else
                defaultView


viewTodoContent content =
    if String.isEmpty content then
        txtC "flex-auto gray" "<empty>"

    else
        txtC "flex-auto" content
