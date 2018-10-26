module Main exposing (main)

import BasicsX exposing (defaultEmptyStringTo, flip, ter, unpackResult, unwrapMaybe, when)
import Browser
import Browser.Dom
import Browser.Events
import Dict
import FeatherIcons
import HotKey
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Json.Decode as D
import Json.Encode as E
import Log
import MagicMenu exposing (MagicMenu)
import Port
import Random
import Store exposing (Item, Store, resetCache)
import Task
import Toasty
import Toasty.Defaults
import Todo exposing (TodoAttrs, TodoItem, TodoStore)
import UI exposing (..)
import Update2
import Update3
import UpdateReturn exposing (andThen, foldlOutMsgList, pure)
import WheelEvent exposing (WheelEvent)



---- MODEL ----


type Mode
    = ListTodoMode
    | EditContentMode Store.Id Todo.Content


type alias Model =
    { magicMenu : MagicMenu
    , toasties : Toasty.Stack Log.Line
    , todoStore : TodoStore
    , mode : Mode
    , listFilter : Todo.ListFilter
    }


type alias Mills =
    Int


type alias Flags =
    { now : Mills, todos : E.Value }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( maybeWarn, todoStore ) =
            Store.load Todo.storeConfig flags.todos
    in
    pure
        { magicMenu = MagicMenu.initial
        , toasties = Toasty.initialState
        , todoStore = todoStore
        , mode = ListTodoMode
        , listFilter = Todo.Active
        }
        |> andThenUpdate (maybeWarn |> unwrapMaybe NoOp Warn)


editContentMode todo =
    EditContentMode todo.meta.id todo.attrs.content



---- UPDATE ----


andThenUpdate msg =
    andThen (update msg)


type Msg
    = NoOp
    | Warn Log.Line
    | ToastyMsg (Toasty.Msg Log.Line)
    | SetMode Mode
    | FocusDomId String
    | MagicMenuMsg MagicMenu.Msg
    | TodoStoreMsg (Store.Msg TodoAttrs)
    | StartEditing TodoItem
    | AddClicked
    | EditClicked TodoItem
    | ContentChanged Todo.Content
    | EndEditMode


handleMagicMenuMessage =
    Update2.lift
        .magicMenu
        (\magicMenu model -> { model | magicMenu = magicMenu })
        MagicMenuMsg
        MagicMenu.update


handleTodoStoreMsg msg model =
    Update3.lift
        .todoStore
        (\sub m -> { m | todoStore = sub })
        TodoStoreMsg
        (Store.update Todo.storeConfig)
        msg
        model
        |> foldlOutMsgList handleTodStoreOutMsg


handleTodStoreOutMsg outMsg model =
    case outMsg of
        Store.InsertedOutMsg newTodo ->
            update (StartEditing newTodo) model

        Store.ModifiedOutMsg updatedTodo ->
            let
                newMode =
                    case model.mode of
                        EditContentMode id content ->
                            if updatedTodo.meta.id == id then
                                editContentMode updatedTodo

                            else
                                model.mode

                        ListTodoMode ->
                            model.mode
            in
            update (SetMode newMode) model


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        Warn logMessages ->
            ( model, Log.warn "Main" logMessages )
                |> Toasty.addPersistentToast Toasty.config ToastyMsg ("Main" :: logMessages)

        ToastyMsg msg ->
            Toasty.update Toasty.config ToastyMsg msg model

        FocusDomId domId ->
            ( model
            , Browser.Dom.focus domId
                |> Task.attempt
                    (unpackResult
                        (\_ -> Warn [ "Focus Error: #", domId, " NotFound" ])
                        (\_ -> NoOp)
                    )
            )

        SetMode mode ->
            pure { model | mode = mode }

        MagicMenuMsg msg ->
            handleMagicMenuMessage msg model

        TodoStoreMsg msg ->
            handleTodoStoreMsg msg model

        StartEditing todo ->
            update (SetMode <| editContentMode todo) model
                |> andThen (update <| FocusDomId modalTodoInputDomId)

        AddClicked ->
            update (TodoStoreMsg <| Store.insertNew Todo.defaultValue) model

        EditClicked todo ->
            update (StartEditing todo) model

        ContentChanged newContent ->
            case model.mode of
                EditContentMode id _ ->
                    update
                        (TodoStoreMsg <|
                            Store.updateItem Todo.storeConfig
                                id
                                (Todo.SetContent newContent)
                                model.todoStore
                        )
                        model

                ListTodoMode ->
                    pure model

        EndEditMode ->
            case model.mode of
                EditContentMode id _ ->
                    pure { model | mode = ListTodoMode }

                ListTodoMode ->
                    pure model



---- Subscriptions


subscriptions model =
    Sub.batch
        [ MagicMenu.subscriptions model.magicMenu |> Sub.map MagicMenuMsg
        ]



---- VIEW ----


mockActions =
    [ FeatherIcons.home
    , FeatherIcons.twitter
    , FeatherIcons.scissors
    , FeatherIcons.edit
    , FeatherIcons.moon
    ]
        |> List.map (\icon -> MagicMenu.Action icon NoOp)
        |> (::) (MagicMenu.Action FeatherIcons.trash2 (TodoStoreMsg resetCache))
        |> (::) (MagicMenu.Action FeatherIcons.filePlus AddClicked)


view : Model -> Html Msg
view model =
    UI.root
        [ viewToolbar model
        , div [ class "w-100 flex flex-column justify-center items-center vs3 pv3" ]
            [ button [ onClick AddClicked ] [ text "add" ]
            , viewTodoList model
            ]
        , viewModal model

        --        , Html.map TodosMsg <|
        --            div [ class "w-100 flex flex-column justify-center items-center vs3 pv3" ]
        --                [ Todos.view model.todos ]
        , div [ class "w-100 flex flex-column justify-center items-center" ]
            [ MagicMenu.view mockActions MagicMenuMsg model.magicMenu ]
        , Toasty.view Toasty.config renderToast ToastyMsg model.toasties
        ]


renderToast : Log.Line -> Html Msg
renderToast toast =
    div [] [ text (toast |> String.join " ") ]


viewModal model =
    case model.mode of
        EditContentMode id content ->
            viewNewTodoModal id content

        ListTodoMode ->
            text ""


modalTodoInputDomId =
    "modal-todo-content-input"


viewNewTodoModal todoId content =
    backdrop []
        [ div
            [ class "bg-white br4 shadow-1 pa3 measure w-100"
            ]
            [ div [ class "w-100 flex" ]
                [ input
                    [ id modalTodoInputDomId
                    , class "flex-auto pa3"
                    , value content
                    , onInput ContentChanged
                    , Html.Events.on "keydown"
                        (D.map
                            (\ke ->
                                case ke of
                                    ( [], "Enter" ) ->
                                        EndEditMode

                                    _ ->
                                        NoOp
                            )
                            HotKey.decoder
                        )
                    ]
                    []
                ]
            ]
        ]


filterTodoItem model todo =
    Todo.matchesFilter model.listFilter todo


viewTodoList : Model -> Html Msg
viewTodoList model =
    let
        todoList =
            model.todoStore
                |> Store.toPairs
                |> List.filter (Tuple.second >> filterTodoItem model)
                |> List.map (Tuple.mapSecond viewTodoItem)
    in
    Html.Keyed.node "div" [ class "w-100 measure-wide items-center" ] todoList


type alias TodoView msg =
    { content : String, done : Bool, editContentMsg : msg }


todoView : TodoItem -> TodoView Msg
todoView todo =
    TodoView
        (defaultEmptyStringTo "<empty>" <| Todo.content todo)
        (Todo.isCompleted todo)
        (EditClicked todo)


viewTodoItem : Store.Item TodoAttrs -> Html Msg
viewTodoItem todo =
    let
        content : String
        content =
            when String.isEmpty (\_ -> "<empty>") (Todo.content todo)

        completed =
            Todo.isCompleted todo
    in
    div
        [ class "pa3 w-100 pointer bb b--light-gray"
        , onClick <| EditClicked todo
        ]
        [ txt content ]



--- Toolbar


viewToolbar model =
    UI.toolbar
        [ txtC "b pa3" "ELM Experiment Store"
        , spacer

        --        , viewTabs (Todos.getFilters model.todos |> List.map viewTab)
        , spacer
        ]



--onClickFilter =
--    Todos.SetFilter >> TodosMsg >> onClick
--
--viewTab ( active, filter, labelText ) =
--    txtA
--        [ onClickFilter filter
--        , class "pointer b ph3 pv2"
--        , classList [ ( "bw2 bb b--blue", active ) ]
--        ]
--        labelText


viewTabs =
    div [ class "flex bw2 bt b--transparent" ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = {- Step.asUpdateFunction -} update
        , subscriptions = subscriptions
        }
