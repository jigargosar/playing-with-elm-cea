module Main exposing (main)

import BasicsX exposing (Millis, defaultEmptyStringTo, everyXSeconds, flip, ter, unpackResult, unwrapMaybe, when)
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
import Mode exposing (Mode)
import Port
import Random
import Store exposing (Id, Item, Store, resetCache)
import Task
import Todo exposing (TodoAttrs)
import TodoStore exposing (TodoStore)
import UI exposing (..)
import Update2
import Update3
import UpdateReturn exposing (Update3Config, andThen, foldlOutMsgList, pure, update3)
import WheelEvent exposing (WheelEvent)



---- MODEL ----


type alias Model =
    { lastTickAt : Int
    , magicMenu : MagicMenu
    , todoStore : TodoStore
    , mode : Mode
    , listFilter : ListFilter.Model
    }


type alias Flags =
    { now : Millis, todos : E.Value }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( maybeWarn, todoStore ) =
            Store.load Todo.storeConfig flags.todos
    in
    pure
        { lastTickAt = flags.now
        , magicMenu = MagicMenu.initial
        , todoStore = todoStore
        , mode = Mode.init
        , listFilter = ListFilter.init flags.now
        }
        |> andThenUpdate (maybeWarn |> unwrapMaybe NoOp Warn)


isFilterSelected filter =
    .listFilter >> ListFilter.isSelected filter



---- UPDATE ----


andThenUpdate msg =
    andThen (update msg)


type Msg
    = NoOp
    | Warn Log.Line
    | Tick Millis
    | ListFilterMsg ListFilter.Msg
    | FocusDomId String
    | MagicMenuMsg MagicMenu.Msg
    | TodoStoreMsg TodoStore.Msg
    | TodoStoreOutMsg (Store.OutMsg TodoAttrs)
    | AddClicked
    | EditClicked TodoStore.Item
    | TodoUpdateMsg Id Todo.Msg
    | AddNewWithContent Todo.Content
    | ModeMsg Mode.Msg
    | ModeOutMsg Mode.OutMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        Warn logMessages ->
            ( model, Log.warn "Main" logMessages )

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

        ListFilterMsg msg ->
            Update2.lift
                .listFilter
                (\s b -> { b | listFilter = s })
                ListFilterMsg
                ListFilter.update
                msg
                model

        MagicMenuMsg msg ->
            Update2.lift
                .magicMenu
                (\s b -> { b | magicMenu = s })
                MagicMenuMsg
                MagicMenu.update
                msg
                model

        AddClicked ->
            update (ModeMsg <| Mode.StartAdding) model

        EditClicked todo ->
            update (ModeMsg <| Mode.StartEditing todo) model

        AddNewWithContent content ->
            update (TodoStoreMsg <| TodoStore.insertNewWithContent content) model

        TodoUpdateMsg id msg ->
            update (TodoStoreMsg <| Store.updateItem id msg) model

        TodoStoreMsg msg ->
            updateTodoStore msg model

        TodoStoreOutMsg msg ->
            case msg of
                Store.InsertedOutMsg newTodo ->
                    update (Warn [ "Store.InsertedOutMsg: unused" ]) model

                Store.ModifiedOutMsg updatedTodo ->
                    update (Warn [ "Store.ModifiedOutMsg: unused" ]) model

        ModeMsg msg ->
            updateMode msg model

        ModeOutMsg msg ->
            case msg of
                Mode.TodoContentUpdatedOutMsg id newContent ->
                    update (TodoUpdateMsg id (Todo.SetContent newContent)) model

                Mode.FocusDomIdOutMsg domId ->
                    update (FocusDomId domId) model

                Mode.AddTodoWithContentOutMsg content ->
                    update (AddNewWithContent content) model


updateMode : Mode.Msg -> Model -> ( Model, Cmd Msg )
updateMode =
    let
        config : Update3Config Mode Mode.Msg Mode.OutMsg Model Msg
        config =
            { get = .mode
            , set = \s b -> { b | mode = s }
            , toMsg = ModeMsg
            , update = Mode.update
            , toOutMsg = ModeOutMsg
            , updateOutMsg = update
            }
    in
    update3 config


updateTodoStore : TodoStore.Msg -> Model -> ( Model, Cmd Msg )
updateTodoStore =
    let
        config : Update3Config TodoStore TodoStore.Msg TodoStore.OutMsg Model Msg
        config =
            { get = .todoStore
            , set = \s b -> { b | todoStore = s }
            , toMsg = TodoStoreMsg
            , update = TodoStore.update
            , toOutMsg = TodoStoreOutMsg
            , updateOutMsg = update
            }
    in
    update3 config



---- Subscriptions


subscriptions model =
    Sub.batch
        [ MagicMenu.subscriptions model.magicMenu |> Sub.map MagicMenuMsg
        , everyXSeconds 10 Tick
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
                    , classList [ ( "b--transparent", isFilterSelected filter model |> not ) ]
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
        , Mode.viewModal model |> Html.map ModeMsg
        ]


modalTodoInputDomId =
    "modal-todo-content-input"


getListsTuple : Model -> ( List TodoStore.Item, Int )
getListsTuple model =
    ListFilter.getFilteredLists model.lastTickAt (Store.items model.todoStore) model.listFilter
        |> Tuple.mapSecond List.length


viewTodoList : Model -> Html Msg
viewTodoList model =
    let
        ( primaryList, secondaryListLength ) =
            getListsTuple model

        viewPrimaryListKeyed =
            primaryList
                |> List.sortBy Store.itemCreatedAt
                |> List.map (\todo -> ( todo.meta.id, viewTodoItem (createTodoViewModel model todo) ))

        viewMore =
            row "justify-center pointer"
                [ onClick <| ListFilterMsg ListFilter.UpdateModifiedAtToNow ]
                [ txt "more", txt <| String.fromInt secondaryListLength ]
    in
    div [ class "w-100 measure-wide" ]
        [ boolHtml (secondaryListLength > 0) viewMore
        , Html.Keyed.node "div" [] viewPrimaryListKeyed
        ]


type alias TodoViewModel msg =
    { content : String
    , isCompleted : Bool
    , editContentMsg : msg
    , updateMsg : Todo.Msg -> msg
    , now : Int
    }


createTodoViewModel : Model -> TodoStore.Item -> TodoViewModel Msg
createTodoViewModel model todo =
    TodoViewModel
        (defaultEmptyStringTo "<empty>" <| Todo.content todo)
        (Todo.isCompleted todo)
        (EditClicked todo)
        (TodoUpdateMsg todo.meta.id)
        model.lastTickAt


viewTodoItem : TodoViewModel msg -> Html msg
viewTodoItem todoVM =
    div
        [ class "pa3 w-100  bb b--light-gray"
        , classList [ ( "strike", todoVM.isCompleted ) ]
        ]
        [ row ""
            []
            [ if todoVM.isCompleted then
                fBtn FeatherIcons.checkCircle <| todoVM.updateMsg Todo.UnmarkCompleted

              else
                fBtn FeatherIcons.circle <| todoVM.updateMsg Todo.MarkCompleted
            , div [ class "flex-grow-1 pointer", onClick todoVM.editContentMsg ] [ txt todoVM.content ]
            , boolHtml
                (not todoVM.isCompleted)
                (fBtn FeatherIcons.clock <| todoVM.updateMsg <| Todo.SetScheduledAt (todoVM.now + 1000 * 60))
            ]
        ]



--- Toolbar


viewToolbar model =
    UI.toolbar
        [ txtC "b pa3" "ELM Experiment Store"
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = {- Step.asUpdateFunction -} update
        , subscriptions = subscriptions
        }
