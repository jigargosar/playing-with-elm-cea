module Todos exposing (Msg(..), Todos, generator, subscriptions, update, view)

import Array
import BasicsX exposing (flip)
import Browser.Dom
import Browser.Events
import Collection exposing (Collection)
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


type alias Cursor =
    Int


type alias Model =
    { mode : Mode, cursor : Int, collection : TodoCollection }


type alias Todos =
    Model


generator : E.Value -> Random.Generator Model
generator enc =
    Collection.generator Todo.decoder enc
        |> Random.map (Model ListMode 0)


setMode : Mode -> Model -> Model
setMode mode model =
    { model | mode = mode }


get : Todo.Id -> Model -> Maybe Todo
get id =
    .collection >> Collection.get id


setCollection : TodoCollection -> Model -> Model
setCollection collection model =
    { model | collection = collection }


getTodoList model =
    let
        list =
            model
                |> .collection
                >> Collection.items
                --        >> List.sortBy (.modifiedAt >> (*) -1)
                >> List.sortBy .createdAt

        currentCursor =
            clamp 0 (List.length list) model.cursor
    in
    ( currentCursor, list )


rotateCursorBy offset model =
    let
        ( currentCursor, length ) =
            getTodoList model
                |> Tuple.mapSecond List.length
    in
    if length == 0 then
        model

    else
        { model | cursor = modBy length (currentCursor + offset) }


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
                , focusTodo todo
                ]
            )

        SetAndCacheCollection collection ->
            ( setCollection collection model, Port.cacheTodoC (Collection.encode Todo.encode collection) )

        SetCursor newCursor ->
            ( { model | cursor = newCursor }, Cmd.none )

        KeyDown key ->
            case key of
                "ArrowDown" ->
                    ( rotateCursorBy 1 model, Cmd.none )

                "ArrowUp" ->
                    ( rotateCursorBy -1 model, Cmd.none )

                "Enter" ->
                    switchModeToEditTodoAtCursor model

                _ ->
                    ( model, Cmd.none )

        StartEditing id ->
            switchModeToEditTodoWithId id model

        EndEditing msg ->
            case model.mode of
                ListMode ->
                    ( model, warn [ "EndEditing in List mode", msg ] )

                EditMode id content ->
                    ( setMode ListMode model
                    , model.collection
                        |> Collection.updateWith id (Todo.setContent content)
                        |> Task.perform SetAndCacheCollection
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


focusTodo todo =
    let
        domId =
            todoInputDomId todo
    in
    Browser.Dom.focus domId
        |> Task.attempt
            (\result ->
                case result of
                    Ok _ ->
                        NoOp

                    Err _ ->
                        LogWarn [ "Browser.Dom.focus", domId, "not found" ]
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
            ( model |> setMode (initEditModeWithTodo todo), focusTodo todo )

        EditMode _ _ ->
            ( model |> setMode (initEditModeWithTodo todo), focusTodo todo )


switchModeToEditTodoAtCursor model =
    getTodoAtCursor model
        |> Maybe.map (\todo -> switchModeToEditTodo todo model)
        |> Maybe.withDefault ( model, Cmd.none )


getTodoAtCursor model =
    let
        ( c, l ) =
            getTodoList model
    in
    Array.fromList l |> Array.get c



--- View


view : Model -> Html Msg
view =
    viewList


viewList : Model -> Html Msg
viewList model =
    let
        ( computedCursor, list ) =
            getTodoList model
    in
    list
        |> List.indexedMap (\index todo -> ( todo.id, viewTodo model (computedCursor == index) index todo ))
        >> Html.Keyed.node "div" [ class "w-100 measure-narrow" ]


todoInputDomId todo =
    "todo-content-input-" ++ todo.id


viewTodo : Model -> Bool -> Int -> Todo -> Html Msg
viewTodo model atCursor index todo =
    let
        defaultView =
            row " bb b--moon-gray lh-copy"
                [ classList [ ( "bg-yellow", atCursor ) ] ]
                [ viewTodoContent
                    (SetCursor index)
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
                        , onBlur <| EndEditing "blur"
                        , Html.Events.on "keydown"
                            (D.map
                                (\key ->
                                    case key of
                                        "Enter" ->
                                            EndEditing "enter"

                                        _ ->
                                            NoOp
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
