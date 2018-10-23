module Todos exposing (Msg(..), Todos, generator, getFilters, subscriptions, update, view)

import Array
import BasicsX exposing (flip, ter)
import Browser.Dom
import Browser.Events
import Collection exposing (Collection)
import Cursor exposing (Cursor)
import HotKey exposing (SoftKey(..))
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
    | EditContentMode Todo.Id Todo.Content


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
    | KeyDown HotKey.Event
    | SetFilter Todo.State


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (D.map KeyDown HotKey.decoder)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    (case message of
        NoOp ->
            ( model, Cmd.none )

        LogWarn strList ->
            ( model, warn strList )

        NewAdded ( todo, collection ) ->
            ( setCollection collection model
                |> setMode (EditContentMode todo.id (Todo.getContent todo))
            , Cmd.batch
                [ Port.cacheTodoC (Collection.encode Todo.encode collection)
                , focusTodoInput todo
                ]
            )

        SetAndCacheCollection collection ->
            ( setCollection collection model, Port.cacheTodoC (Collection.encode Todo.encode collection) )

        SetCursor newCursor ->
            ( { model | cursor = newCursor }, Cmd.none )

        KeyDown ke ->
            case model.mode of
                ListMode ->
                    case ke of
                        ( [], "ArrowDown" ) ->
                            cycleCursorByOffsetAndFocus 1 model

                        ( [], "ArrowUp" ) ->
                            cycleCursorByOffsetAndFocus -1 model

                        ( [], "ArrowLeft" ) ->
                            onChangeStateRequest Left model

                        ( [], "ArrowRight" ) ->
                            onChangeStateRequest Right model

                        ( [ Meta ], "ArrowLeft" ) ->
                            onChangeFilterRequest Left model

                        ( [ Meta ], "ArrowRight" ) ->
                            onChangeFilterRequest Right model

                        ( [], "Enter" ) ->
                            switchModeToEditTodoAtCursor model

                        _ ->
                            ( model, Cmd.none )

                EditContentMode _ _ ->
                    case ke of
                        ( [], "Enter" ) ->
                            switchModeToEditTodoAtCursor model

                        _ ->
                            ( model, Cmd.none )

        StartEditing id ->
            switchModeToEditTodoWithId id model

        EndEditing msg ->
            case model.mode of
                ListMode ->
                    ( model, warn [ "EndEditing in List mode", msg ] )

                EditContentMode id content ->
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

                EditContentMode id _ ->
                    ( setMode (EditContentMode id newContent) model
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
            changeFilterTo newFilter model
    )
        |> andThenFocusTodoAtCursor model


changeFilterTo newFilter model =
    ( { model | filter = newFilter }, Cmd.none )


updateTodo id fn model =
    model.collection
        |> Collection.updateWith id fn
        |> Task.perform SetAndCacheCollection


andThenFocusTodoAtCursor oldModel ( newModel, cmd ) =
    if oldModel /= newModel then
        ( newModel, Cmd.batch [ cmd, focusTodoAtCursor newModel ] )

    else
        ( newModel, cmd )


focusTodoAtCursor model =
    case model.mode of
        ListMode ->
            getTodoAtCursor model
                |> Maybe.map focusTodoItem
                |> Maybe.withDefault Cmd.none

        _ ->
            Cmd.none


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
    , focusTodoAtCursor model
    )


warn : Log.Messages -> Cmd msg
warn =
    Log.warn "Todos.elm"


initEditContentModeWithTodo todo =
    EditContentMode todo.id <| Todo.getContent todo


switchModeToEditTodoWithId id model =
    case get id model of
        Just todo ->
            switchModeToEditTodo todo model

        Nothing ->
            ( model, warn [ "startEditingAndFocus", id, "Todo Not Found" ] )


switchModeToEditTodo todo model =
    case model.mode of
        ListMode ->
            ( model |> setMode (initEditContentModeWithTodo todo), focusTodoInput todo )

        EditContentMode id oldContent ->
            ( model |> setMode (initEditContentModeWithTodo todo)
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


type StateDirection
    = Left
    | Right


computeNextState direction state =
    case direction of
        Left ->
            case state of
                Todo.Completed ->
                    Todo.Active

                _ ->
                    Todo.Scheduled

        Right ->
            case state of
                Todo.Scheduled ->
                    Todo.Active

                _ ->
                    Todo.Completed


onChangeStateRequest direction model =
    getTodoAtCursor model
        |> Maybe.map
            (\todo ->
                let
                    nextState =
                        computeNextState direction (Todo.getState todo)
                in
                ( model, updateTodo todo.id (Todo.changeStateTo nextState) model )
            )
        |> Maybe.withDefault ( model, Cmd.none )


onChangeFilterRequest direction model =
    changeFilterTo (computeNextState direction model.filter) model


cmdSetStateAtCursorTo state model =
    getTodoAtCursor model
        |> Maybe.map (\todo -> updateTodo todo.id (Todo.changeStateTo state) model)
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

        EditContentMode id content ->
            if todo.id == id then
                flexV []
                    [ input
                        [ Html.Attributes.id <| todoInputDomId todo
                        , class "pa3 lh-copy"
                        , value content
                        , onInput ContentChanged
                        , onBlur <| EndEditing "blur"
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
