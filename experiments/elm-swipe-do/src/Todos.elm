module Todos exposing (Msg(..), Todos, generator, update, view)

import BasicsX exposing (flip)
import Browser.Dom
import Collection exposing (Collection)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Log
import Port
import Process
import Random
import Task exposing (Task)
import Todo exposing (Todo)
import UI exposing (flexV, row, txtA, txtC)


type Mode
    = List
    | Edit Todo.Id Todo.Content


type alias TodoCollection =
    Collection Todo


type alias Model =
    { mode : Mode, collection : TodoCollection }


type alias Todos =
    Model


setMode : Mode -> Model -> Model
setMode mode model =
    { model | mode = mode }


get : Todo.Id -> Model -> Maybe Todo
get id =
    .collection >> Collection.get id


setCollection : TodoCollection -> Model -> Model
setCollection collection model =
    { model | collection = collection }


getTodoList =
    .collection
        >> Collection.items
        --        >> List.sortBy (.modifiedAt >> (*) -1)
        >> List.sortBy .createdAt


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
    | SetAndCacheCollection TodoCollection
    | ContentChanged Todo.Content
    | LogWarn (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LogWarn strList ->
            ( model, warn strList )

        NewAdded ( todo, collection ) ->
            ( setCollection collection model
                |> setMode (Edit todo.id (Todo.getContent todo))
            , Cmd.batch
                [ Port.cacheTodoC (Collection.encode Todo.encode collection)
                , focusTodo todo
                ]
            )

        SetAndCacheCollection collection ->
            ( setCollection collection model, Port.cacheTodoC (Collection.encode Todo.encode collection) )

        ContentChanged newContent ->
            case model.mode of
                List ->
                    ( model, warn [ "ContentChanged in List mode" ] )

                Edit id _ ->
                    ( setMode (Edit id newContent) model
                    , model.collection
                        |> Collection.updateWith id (Todo.setContent newContent)
                        |> Task.perform SetAndCacheCollection
                    )

        StartEditing todoId ->
            case model.mode of
                List ->
                    startEditingAndFocus todoId model

                Edit _ _ ->
                    startEditingAndFocus todoId model

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


focusTodo todo =
    let
        domId =
            todoInputDomId todo
    in
    Browser.Dom.focus domId
        |> Task.attempt (\_ -> LogWarn [ "Browser.Dom.focus", domId, "not found" ])


warn : Log.Messages -> Cmd msg
warn =
    Log.warn "Todos.elm"


initEditModeWithTodo todo =
    Edit todo.id <| Todo.getContent todo


startEditingAndFocus id model =
    case get id model of
        Just todo ->
            ( model |> setMode (initEditModeWithTodo todo), focusTodo todo )

        Nothing ->
            ( model, warn [ "startEditingAndFocus", id, "Todo Not Found" ] )


view : Model -> Html Msg
view =
    viewList


viewList : Model -> Html Msg
viewList model =
    model |> getTodoList >> List.map (viewTodo model.mode) >> div [ class "w-100 measure-narrow vs3" ]


todoInputDomId todo =
    "todo-content-input-"


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
                flexV []
                    [ input
                        [ Html.Attributes.id <| todoInputDomId todo
                        , value content
                        , onInput ContentChanged
                        ]
                        []
                    ]

            else
                defaultView


viewTodoContent onClick_ content =
    if String.isEmpty content then
        txtA [ onClick onClick_, class "flex-auto gray" ] "<empty>"

    else
        txtA [ onClick onClick_, class "flex-auto" ] content
