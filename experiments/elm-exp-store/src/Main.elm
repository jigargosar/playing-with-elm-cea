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
import ListFilter exposing (Filter)
import Log
import MagicMenu exposing (MagicMenu)
import Port
import Random
import Store exposing (Id, Item, Store, resetCache)
import Task
import Time
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
    { lastTickAt : Int
    , magicMenu : MagicMenu
    , toasties : Toasty.Stack Log.Line
    , todoStore : TodoStore
    , mode : Mode
    , listFilter : ListFilter.Model
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
        { lastTickAt = flags.now
        , magicMenu = MagicMenu.initial
        , toasties = Toasty.initialState
        , todoStore = todoStore
        , mode = ListTodoMode
        , listFilter = ListFilter.init flags.now
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
    | Tick Mills
    | SetMode Mode
    | ListFilterMsg ListFilter.Msg
    | FocusDomId String
    | MagicMenuMsg MagicMenu.Msg
    | TodoStoreMsg (Store.Msg TodoAttrs)
    | StartEditing TodoItem
    | AddClicked
    | EditClicked TodoItem
    | UpdateTodoId Id Todo.Msg
    | ContentChanged Todo.Content
    | EndEditMode


handleMagicMenuMsg =
    Update2.lift
        .magicMenu
        (\magicMenu model -> { model | magicMenu = magicMenu })
        MagicMenuMsg
        MagicMenu.update


handleListFilterMsg =
    Update2.lift
        .listFilter
        (\listFilter model -> { model | listFilter = listFilter })
        ListFilterMsg
        ListFilter.update


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

        Tick millis ->
            pure { model | lastTickAt = millis }

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

        ListFilterMsg msg ->
            handleListFilterMsg msg model

        MagicMenuMsg msg ->
            handleMagicMenuMsg msg model

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

        UpdateTodoId id msg ->
            update
                (TodoStoreMsg <|
                    Store.updateItem Todo.storeConfig
                        id
                        msg
                        model.todoStore
                )
                model

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
        , Time.every (1000 * 10) (Time.posixToMillis >> Tick)
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
    let
        onClickSetFilter =
            onClick << ListFilterMsg << ListFilter.SwitchFilterTo

        filterBtn filter label =
            button
                [ onClickSetFilter filter
                , class "pv2 "
                ]
                [ txtA
                    [ class "bb bw1"
                    , classList [ ( "b--transparent", model.listFilter |> ListFilter.isSelected filter |> not ) ]
                    ]
                    label
                ]
    in
    UI.root
        [ viewToolbar model
        , div [ class "w-100 flex flex-column justify-center items-center vs3 pv3" ]
            [ row ""
                []
                [ button [ onClick AddClicked ] [ text "add" ]
                , filterBtn ListFilter.Future "Future"
                , filterBtn ListFilter.Active "Active"
                , filterBtn ListFilter.Completed "Completed"
                ]
            , viewTodoList model
            ]
        , div [ class "w-100 flex flex-column justify-center items-center" ]
            [ MagicMenu.view mockActions MagicMenuMsg model.magicMenu ]
        , viewModal model
        , Toasty.view Toasty.config renderToast ToastyMsg model.toasties
        ]


renderToast : Log.Line -> Html Msg
renderToast toast =
    div [] [ text (toast |> String.join " ") ]


viewModal model =
    case model.mode of
        EditContentMode id content ->
            viewEditContentModal id content

        ListTodoMode ->
            text ""


modalTodoInputDomId =
    "modal-todo-content-input"


viewEditContentModal todoId content =
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

                                    ( [], "Escape" ) ->
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


viewTodoList : Model -> Html Msg
viewTodoList model =
    let
        todoList =
            model.todoStore
                |> Store.items

        viewPrimaryListKeyed =
            todoList
                |> List.filter (ListFilter.matchesSelectedIn model.listFilter)
                |> List.sortBy Store.itemCreatedAt
                |> List.map (\todo -> ( todo.meta.id, viewTodoItem model todo ))

        secondaryList =
            todoList
                |> List.filter (ListFilter.matchesSelectedWithReferenceTimeIn model.listFilter model.lastTickAt)

        secondaryListLength =
            List.length secondaryList

        viewMore =
            row "justify-center" [] [ txt "more", txt <| String.fromInt secondaryListLength ]
    in
    div [ class "w-100 measure-wide" ]
        [ boolHtml (secondaryListLength > 0) viewMore
        , Html.Keyed.node "div" [] viewPrimaryListKeyed
        ]


type alias TodoView msg =
    { content : String
    , isCompleted : Bool
    , editContentMsg : msg
    , updateMsg : Todo.Msg -> msg
    , now : Int
    }


todoView : Model -> TodoItem -> TodoView Msg
todoView model todo =
    TodoView
        (defaultEmptyStringTo "<empty>" <| Todo.content todo)
        (Todo.isCompleted todo)
        (EditClicked todo)
        (UpdateTodoId todo.meta.id)
        model.lastTickAt


viewTodoItem : Model -> Store.Item TodoAttrs -> Html Msg
viewTodoItem model todo =
    let
        vm =
            todoView model todo
    in
    div
        [ class "pa3 w-100  bb b--light-gray"
        , classList [ ( "strike", vm.isCompleted ) ]
        ]
        [ row ""
            []
            [ if vm.isCompleted then
                fBtn FeatherIcons.checkCircle <| vm.updateMsg Todo.UnmarkCompleted

              else
                fBtn FeatherIcons.circle <| vm.updateMsg Todo.MarkCompleted
            , div [ class "flex-grow-1 pointer", onClick vm.editContentMsg ] [ txt vm.content ]
            , boolHtml
                (not vm.isCompleted)
                (fBtn FeatherIcons.clock <| vm.updateMsg <| Todo.SetScheduledAt (vm.now + 1000 * 60))
            ]
        ]



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
