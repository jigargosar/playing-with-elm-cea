module Todos exposing (Msg(..), Todos, generator, getFilters, subscriptions, update, view)

import Array
import BasicsX exposing (flip, maybeBool, ter, unwrapMaybe)
import Browser.Dom
import Browser.Events
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
import Schedule exposing (Schedule)
import Set exposing (Set)
import Task exposing (Task)
import Todo exposing (Todo)
import TodoCollection exposing (TodoCollection)
import UI exposing (flexV, row, txtA, txtC)


type Mode
    = ListMode
    | EditContentMode Todo.Id Todo.Content
    | EditScheduleMode Todo.Id


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
generator =
    TodoCollection.generator
        >> Random.map (Tuple.mapFirst <| Model ListMode 0 Todo.Active)


setMode : Mode -> Model -> Model
setMode mode model =
    { model | mode = mode }


get : Todo.Id -> Model -> Maybe Todo
get id =
    .collection >> TodoCollection.get id


setCollection : TodoCollection -> Model -> Model
setCollection collection model =
    { model | collection = collection }


getCursorTodoList model =
    model.cursor
        |> Cursor.get
            (model.collection
                |> TodoCollection.items
                |> List.filter (Todo.stateEq model.filter)
                |> List.sortBy .createdAt
            )


type Msg
    = NoOp
    | NewClicked
    | StartEditingContent Todo.Id
    | EndEditing String
    | Reset
    | NewAdded ( Todo, TodoCollection )
    | SetAndCacheCollection TodoCollection
    | ContentChanged Todo.Content
    | LogWarn (List String)
    | SetCursor Cursor
    | KeyDown HotKey.Event
    | SetFilter Todo.State
    | ScheduleKindChanged Schedule.Kind


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
                [ TodoCollection.cacheCmd collection
                , focusTodoInput todo
                ]
            )

        SetAndCacheCollection collection ->
            ( setCollection collection model, TodoCollection.cacheCmd collection )

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

                        _ ->
                            ( model, Cmd.none )

                EditContentMode _ _ ->
                    case ke of
                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartEditingContent id ->
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

                EditScheduleMode id ->
                    Debug.todo "Implement end editing for schedule"

        ContentChanged newContent ->
            case model.mode of
                EditContentMode id _ ->
                    ( setMode (EditContentMode id newContent) model
                    , TodoCollection.setContent id newContent model.collection
                        |> Task.perform SetAndCacheCollection
                    )

                _ ->
                    ( model, warn [ "ContentChanged in Non Edit Mode" ] )

        NewClicked ->
            ( model
            , TodoCollection.addNew model.collection |> Task.perform NewAdded
            )

        Reset ->
            let
                newCollection =
                    TodoCollection.reset model.collection
            in
            ( { model
                | collection = newCollection
              }
            , TodoCollection.cacheCmd newCollection
            )

        SetFilter newFilter ->
            changeFilterTo newFilter model

        ScheduleKindChanged scheduleKind ->
            case model.mode of
                EditScheduleMode id ->
                    ( setMode ListMode model
                    , updateTodo id (Todo.setScheduleKind scheduleKind) model
                    )

                _ ->
                    ( model, warn [ "ScheduleChanged in non EditSchedule Mode" ] )
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


switchModeToEditTodoWithId id model =
    case get id model of
        Just todo ->
            switchModeToEditTodo todo model

        Nothing ->
            ( model, warn [ "switchModeToEditTodoWithId", id, "Todo Not Found" ] )


switchModeToEditTodo todo model =
    case model.mode of
        ListMode ->
            ( model |> setMode (EditContentMode todo.id <| Todo.getContent todo), focusTodoInput todo )

        _ ->
            ( model, warn [ "Invalid mode for switchModeToEditTodo" ] )


switchModeToEditSchedule todo model =
    case model.mode of
        ListMode ->
            ( model |> setMode (EditScheduleMode todo.id), Cmd.none )

        _ ->
            ( model, warn [ "Invalid mode for switchModeToEditSchedule" ] )


switchModeToEditTodoAtCursor model =
    getTodoAtCursor model
        |> Maybe.map (\todo -> switchModeToEditTodo todo model)
        |> Maybe.withDefault ( model, Cmd.none )


getTodoAtCursor model =
    let
        ( cursor, todoList ) =
            getCursorTodoList model
    in
    Array.fromList todoList |> Array.get cursor


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
                startStateChange nextState todo model
            )
        |> Maybe.withDefault ( model, Cmd.none )


startStateChange nextState todo model =
    if nextState == Todo.Scheduled then
        switchModeToEditSchedule todo model

    else
        ( model, updateTodo todo.id (Todo.changeStateTo nextState) model )


onChangeFilterRequest direction model =
    changeFilterTo (computeNextState direction model.filter) model


cmdSetStateAtCursorTo state model =
    getTodoAtCursor model
        |> Maybe.map (\todo -> updateTodo todo.id (Todo.changeStateTo state) model)
        |> Maybe.withDefault Cmd.none



--- View


view : Model -> Html Msg
view model =
    div [ class "w-100 measure-narrow" ]
        [ viewList model
        , viewScheduleOverlay model
        ]


box border scheduleKind lbl =
    div
        [ class <| "flex items-center justify-center b--moon-gray " ++ border
        , style "width" "100px"
        , style "height" "100px"
        , onClick <| ScheduleKindChanged scheduleKind
        , tabindex 0
        ]
        [ text lbl ]


viewScheduleOverlay model =
    case model.mode of
        EditScheduleMode id ->
            div [ class "z-2 absolute absolute--fill bg-black-30  flex items-center justify-center" ]
                [ div [ class "bg-white shadow-1 br4 flex flex-column" ]
                    [ div [ class "flex" ]
                        [ box "bn" (Schedule.Minutes 10) "10 Min"
                        , box "bl br" (Schedule.Minutes 15) "15 Min"
                        , box "bn" (Schedule.Minutes 30) "30 Min"
                        ]
                    , div [ class "flex" ]
                        [ box "bt bb" (Schedule.Hours 1) "1 Hrs"
                        , box "ba" Schedule.LaterToday "Later Today"
                        , box "bt bb" Schedule.Tomorrow "Tomorrow"
                        ]
                    , div [ class "flex" ]
                        [ box "bn" Schedule.WeakEnd "Weak End"
                        , box "bl br" Schedule.NextWeek "Next Week"
                        , box "bn" Schedule.Someday "Someday"
                        ]
                    ]
                ]

        _ ->
            text ""


viewList : Model -> Html Msg
viewList model =
    let
        ( computedCursor, list ) =
            getCursorTodoList model
    in
    list
        |> List.indexedMap
            (\index todo ->
                let
                    isAtCursor =
                        computedCursor == index
                in
                ( todo.id
                , viewTodo model isAtCursor index todo
                )
            )
        >> Html.Keyed.node "div" [ class "" ]


todoInputDomId todo =
    "todo-content-input-" ++ todo.id


todoItemDomId todo =
    todoItemDomIdWithTodoId todo.id


todoItemDomIdWithTodoId id =
    "todo-item-" ++ id


maybeEditingContent todo model =
    case model.mode of
        EditContentMode id content ->
            maybeBool (todo.id == id) content

        _ ->
            Nothing


viewTodo : Model -> Bool -> Int -> Todo -> Html Msg
viewTodo model isAtCursor index todo =
    maybeEditingContent todo model
        |> unwrapMaybe (defaultView isAtCursor index todo) (viewEditingContent todo)


defaultView isAtCursor index todo =
    row " bb b--moon-gray lh-copy"
        [ Html.Attributes.id <| todoItemDomId todo
        , classList
            [ ( "hover-bg-yellow bg-light-yellow", isAtCursor )
            , ( "strike gray", Todo.isCompleted todo )
            ]
        , tabindex <| ter isAtCursor 0 -1
        , onDoubleClick <| StartEditingContent todo.id
        , Html.Events.on "keydown"
            (D.map
                (\ke ->
                    case ke of
                        ( [], "Enter" ) ->
                            StartEditingContent todo.id

                        _ ->
                            NoOp
                )
                HotKey.decoder
            )
        ]
        [ viewTodoContent
            (SetCursor index)
            (Todo.getContent todo)
        ]


viewTodoContent onClick_ content =
    if String.isEmpty content then
        txtA [ onClick onClick_, class "flex-auto pa3 gray" ] "<empty>"

    else
        txtA [ onClick onClick_, class "flex-auto pa3" ] content


viewEditingContent todo content =
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
                                ( EndEditing "enter", False )

                            _ ->
                                ( NoOp, False )
                    )
                    (D.field "key" D.string)
                )
            ]
            []
        ]
