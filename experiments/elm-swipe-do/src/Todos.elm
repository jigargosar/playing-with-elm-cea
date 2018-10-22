module Todos exposing (Msg(..), Todos, generator, update, view)

import BasicsX exposing (flip)
import Browser.Dom
import Collection exposing (Collection)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Port
import Process
import Random
import Task
import Todo exposing (Todo)
import UI exposing (flexV, row, txtA, txtC)


type Mode
    = List
    | Edit Todo.Id Todo.Content


initEditModeWithTodo : Todo -> Mode
initEditModeWithTodo todo =
    Edit todo.id (Todo.getContent todo)


type alias TodoCollection =
    Collection Todo


type alias Model =
    { mode : Mode, collection : TodoCollection }


type alias Todos =
    Model


get : Todo.Id -> Model -> Maybe Todo
get id =
    .collection >> Collection.get id


setCollection : TodoCollection -> Model -> Model
setCollection collection model =
    { model | collection = collection }


setMode : Mode -> Model -> Model
setMode mode model =
    { model | mode = mode }


setModeEditWithTodo : Todo -> Model -> Model
setModeEditWithTodo todo =
    setMode <| initEditModeWithTodo todo


setEditModeWithTodoId : Todo.Id -> Model -> Maybe Model
setEditModeWithTodoId id model =
    get id model |> Maybe.map (\todo -> setModeEditWithTodo todo model)


getTodoList =
    .collection >> Collection.items >> List.sortBy (.modifiedAt >> (*) -1)


generator : E.Value -> Random.Generator Model
generator enc =
    Collection.generator Todo.decoder enc
        |> Random.map (Model List)


type Msg
    = NoOp
    | NewClicked
    | StartEditing Todo.Id
    | Reset
    | NewAdded ( Todo, Collection Todo )
    | LogWarn (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LogWarn strList ->
            ( model, Port.warn ("Todos.elm" :: strList) )

        NewAdded ( todo, collection ) ->
            ( setCollection collection model
                |> setMode (Edit todo.id (Todo.getContent todo))
            , Cmd.batch
                [ Port.cacheTodoC (Collection.encode Todo.encode collection)
                , Browser.Dom.focus "todo-content-input"
                    |> Task.attempt (\_ -> LogWarn [ "Browser.Dom.focus", "todo-content-input" ])
                ]
            )

        StartEditing todoId ->
            case model.mode of
                List ->
                    setEditModeWithTodoId todoId model
                        |> Maybe.map (\newModel -> ( newModel, Cmd.none ))
                        |> Maybe.withDefault
                            ( model
                            , Port.warn [ "StartEditing: todoId not found", todoId ]
                            )

                Edit editingId content ->
                    ( model, Cmd.none )

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


view : Model -> Html Msg
view =
    viewList


viewList : Model -> Html Msg
viewList model =
    model |> getTodoList >> List.map (viewTodo model.mode) >> div [ class "w-100 measure-narrow vs3" ]


viewTodo : Mode -> Todo -> Html Msg
viewTodo mode todo =
    let
        defaultView =
            row "" [] [ viewTodoContent (StartEditing todo.id) (Todo.getContent todo) ]
    in
    case mode of
        List ->
            defaultView

        Edit id content ->
            if todo.id == id then
                flexV [] [ input [ Html.Attributes.id "todo-content-input", value content ] [] ]

            else
                defaultView


viewTodoContent onClick_ content =
    if String.isEmpty content then
        txtA [ onClick onClick_, class "flex-auto gray" ] "<empty>"

    else
        txtA [ onClick onClick_, class "flex-auto" ] content
