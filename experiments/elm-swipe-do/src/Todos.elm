module Todos exposing (Msg(..), Todos, generator, getFilters, subscriptions, update, view)

import Array
import BasicsX exposing (flip, ter)
import Browser.Dom
import Browser.Events
import Collection exposing (Collection)
import Cursor exposing (Cursor)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Json.Decode as D
import Json.Encode as E
import Log
import Port
import Process
import Random
import Set exposing (Set)
import Task exposing (Task)
import Todo exposing (Todo)
import UI exposing (flexV, row, txtA, txtC)


type Mode
    = ListMode
    | EditMode Todo.Id Todo.Content


type alias TodoIds =
    Set Todo.Id


type alias TodoCollection =
    Collection Todo


type alias Model =
    { mode : Mode
    , cursor : Cursor
    , filter : Todo.State
    , collection : TodoCollection
    }


getFilters model =
    [ ( Todo.Scheduled == model.filter, Todo.Scheduled, "Scheduled" )
    , ( Todo.Active == model.filter, Todo.Active, "Active" )
    , ( Todo.Completed == model.filter, Todo.Completed, "Completed" )
    ]


type alias Todos =
    Model


generator : E.Value -> Random.Generator ( Model, Cmd msg )
generator enc =
    Collection.generator Todo.decoder enc
        |> Random.map (Tuple.mapFirst <| Model ListMode 0 Todo.Active)


setMode : Mode -> Model -> Model
setMode mode model =
    { model | mode = mode }


get : Todo.Id -> Model -> Maybe Todo
get id =
    .collection >> Collection.get id


setCollection : TodoCollection -> Model -> Model
setCollection collection model =
    { model | collection = collection }


getCursorTodoList model =
    model.cursor
        |> Cursor.get
            (model.collection
                |> Collection.items
                |> List.filter (Todo.stateEq model.filter)
                |> List.sortBy .createdAt
            )


type Msg
    = NoOp
    | NewClicked
    | StartEditing Todo.Id
    | EndEditing String
    | Reset
    | NewAdded ( Todo, Collection Todo )
    | SetAndCacheCollection TodoCollection
    | ContentChanged Todo.Content
    | LogWarn (List String)
    | SetCursor Cursor
    | KeyDown String
    | SetFilter Todo.State


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown
            (D.map KeyDown (D.field "key" D.string))
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LogWarn strList ->
            ( model, warn strList )

        NewAdded ( todo, collection ) ->
            ( setCollection collection model
                |> setMode (EditMode todo.id (Todo.getContent todo))
            , Cmd.batch
                [ Port.cacheTodoC (Collection.encode Todo.encode collection)
                , focusTodoInput todo
                ]
            )

        SetAndCacheCollection collection ->
            ( setCollection collection model, Port.cacheTodoC (Collection.encode Todo.encode collection) )

        SetCursor newCursor ->
            ( { model | cursor = newCursor }, Cmd.none )

        KeyDown key ->
            case model.mode of
                ListMode ->
                    case key of
                        "ArrowDown" ->
                            cycleCursorByOffsetAndFocus 1 model

                        "ArrowUp" ->
                            cycleCursorByOffsetAndFocus -1 model

                        "ArrowRight" ->
                            ( model, setDoneAtCursor model )

                        "ArrowLeft" ->
                            ( model, setDoneAtCursor model )

                        "Enter" ->
                            switchModeToEditTodoAtCursor model

                        _ ->
                            ( model, Cmd.none )

                EditMode _ _ ->
                    ( model, Cmd.none )

        StartEditing id ->
            switchModeToEditTodoWithId id model

        EndEditing msg ->
            case model.mode of
                ListMode ->
                    ( model, warn [ "EndEditing in List mode", msg ] )

                EditMode id content ->
                    ( setMode ListMode model
                    , Cmd.batch
                        [ updateTodo id (Todo.setContent content) model
                        , focusId <| todoItemDomIdWithTodoId id
                        ]
                    )

        ContentChanged newContent ->
            case model.mode of
                ListMode ->
                    ( model, warn [ "ContentChanged in List mode" ] )

                EditMode id _ ->
                    ( setMode (EditMode id newContent) model
                    , model.collection
                        |> Collection.updateWith id (Todo.setContent newContent)
                        |> Task.perform SetAndCacheCollection
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

        SetFilter newFilter ->
            ( { model | filter = newFilter }, Cmd.none )


updateTodo id fn model =
    model.collection
        |> Collection.updateWith id fn
        |> Task.perform SetAndCacheCollection


focusTodoInput =
    todoInputDomId >> focusId


focusTodoItem =
    todoItemDomId >> focusId


focusId domId =
    Browser.Dom.focus domId
        |> Task.attempt
            (Result.map (always NoOp)
                >> Result.withDefault (LogWarn [ "Browser.Dom.focus", domId, "not found" ])
            )


cycleCursorByOffsetAndFocus offset model =
    let
        ( currentCursor, list ) =
            getCursorTodoList model
    in
    cycleCursorByOffsetAndFocusHelp <|
        { model | cursor = Cursor.cycleByOffset offset list currentCursor }


cycleCursorByOffsetAndFocusHelp model =
    ( model
    , getTodoAtCursor model
        |> Maybe.map focusTodoItem
        |> Maybe.withDefault Cmd.none
    )


warn : Log.Messages -> Cmd msg
warn =
    Log.warn "Todos.elm"


initEditModeWithTodo todo =
    EditMode todo.id <| Todo.getContent todo


switchModeToEditTodoWithId id model =
    case get id model of
        Just todo ->
            switchModeToEditTodo todo model

        Nothing ->
            ( model, warn [ "startEditingAndFocus", id, "Todo Not Found" ] )


switchModeToEditTodo todo model =
    case model.mode of
        ListMode ->
            ( model |> setMode (initEditModeWithTodo todo), focusTodoInput todo )

        EditMode id oldContent ->
            ( model |> setMode (initEditModeWithTodo todo)
            , Cmd.batch
                [ if id == todo.id then
                    warn [ "Reediting todo", todo.id, "with content", Todo.getContent todo, "oldContent", oldContent ]

                  else
                    Cmd.none
                , focusTodoInput todo
                ]
            )


switchModeToEditTodoAtCursor model =
    getTodoAtCursor model
        |> Maybe.map (\todo -> switchModeToEditTodo todo model)
        |> Maybe.withDefault ( model, Cmd.none )


getTodoAtCursor model =
    let
        ( c, l ) =
            getCursorTodoList model
    in
    Array.fromList l |> Array.get c


setDoneAtCursor model =
    getTodoAtCursor model
        |> Maybe.map (\todo -> updateTodo todo.id Todo.markComplete model)
        |> Maybe.withDefault Cmd.none



--- View


view : Model -> Html Msg
view =
    viewList


viewList : Model -> Html Msg
viewList model =
    let
        ( computedCursor, list ) =
            getCursorTodoList model
    in
    list
        |> List.indexedMap (\index todo -> ( todo.id, viewTodo model (computedCursor == index) index todo ))
        >> Html.Keyed.node "div" [ class "w-100 measure-narrow" ]


todoInputDomId todo =
    "todo-content-input-" ++ todo.id


todoItemDomId todo =
    todoItemDomIdWithTodoId todo.id


todoItemDomIdWithTodoId id =
    "todo-item-" ++ id


viewTodo : Model -> Bool -> Cursor -> Todo -> Html Msg
viewTodo model atCursor cursor todo =
    let
        defaultView =
            row " bb b--moon-gray lh-copy"
                [ Html.Attributes.id <| todoItemDomId todo
                , classList
                    [ ( "hover-bg-yellow bg-light-yellow", atCursor )
                    , ( "strike gray", Todo.isCompleted todo )
                    ]
                , tabindex <| ter atCursor 0 -1
                ]
                [ viewTodoContent
                    (SetCursor cursor)
                    (Todo.getContent todo)
                ]
    in
    case model.mode of
        ListMode ->
            defaultView

        EditMode id content ->
            if todo.id == id then
                flexV []
                    [ input
                        [ Html.Attributes.id <| todoInputDomId todo
                        , class "pa3 lh-copy"
                        , value content
                        , onInput ContentChanged

                        --                        , onBlur <| EndEditing "blur"
                        , Html.Events.stopPropagationOn "keydown"
                            (D.map
                                (\key ->
                                    case key of
                                        "Enter" ->
                                            ( EndEditing "enter", True )

                                        _ ->
                                            ( NoOp, False )
                                )
                                (D.field "key" D.string)
                            )
                        ]
                        []
                    ]

            else
                defaultView


viewTodoContent onClick_ content =
    if String.isEmpty content then
        txtA [ onClick onClick_, class "flex-auto pa3 gray" ] "<empty>"

    else
        txtA [ onClick onClick_, class "flex-auto pa3" ] content
