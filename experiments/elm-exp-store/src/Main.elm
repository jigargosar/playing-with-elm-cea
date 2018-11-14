module Main exposing (main)

import AppBar
import Array
import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import Btn
import ContextDialog
import ContextPopup
import ContextStore exposing (Context, ContextId, ContextName, ContextStore)
import ContextTodoList exposing (TodoListConfig)
import Css exposing (..)
import Dict exposing (Dict)
import DomX exposing (DomId, WindowSize)
import FeatherIcons as Icon
import Focus
import HotKey exposing (SoftKey(..))
import Html.Styled as Html exposing (Attribute, Html, button, div, fromUnstyled, span, styled, text)
import Html.Styled.Attributes exposing (class, classList, css, id, style, tabindex)
import Html.Styled.Events exposing (onClick, onDoubleClick)
import Html.Styled.Keyed as HKeyed exposing (node)
import HtmlX
import Icons
import Json.Decode as D
import Json.Encode as E
import JsonCodecX exposing (Value)
import Layer exposing (Layer)
import Log
import Port
import QuickAction
import Random
import Set exposing (Set)
import Sidebar
import Styles exposing (..)
import Svg.Attributes
import Svg.Styled exposing (svg)
import Task
import Time
import TodoDialog
import TodoListSelection
import TodoStore exposing (Todo, TodoContent, TodoId, TodoStore)
import UI exposing (..)
import Update2
import UpdateReturn exposing (..)



---- MODEL ----


type alias Model =
    { todoStore : TodoStore
    , contextStore : ContextStore
    , contextId : ContextId
    , layer : Layer
    , showArchivedContexts : Bool
    , showCompletedTodos : Bool
    , showTempSidebar : Bool
    , selectedIndex : Int
    , windowSize : WindowSize
    }


type alias Flags =
    { now : Millis, windowSize : WindowSize, todos : Value, contexts : Value }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( maybeTodoStoreLogLine, todoStore ) =
            TodoStore.load flags.todos

        ( maybeContextStoreLogLine, contextStore ) =
            ContextStore.load flags.contexts

        model : Model
        model =
            { todoStore = todoStore
            , contextStore = contextStore
            , contextId = ContextStore.defaultId
            , layer = Layer.NoLayer
            , showArchivedContexts = False
            , showCompletedTodos = False
            , showTempSidebar = False
            , selectedIndex = 0
            , windowSize = flags.windowSize
            }
    in
    ( model
    , [ maybeTodoStoreLogLine, maybeContextStoreLogLine ]
        |> List.filterMap (Maybe.map logCmd)
        |> Cmd.batch
    )


getSelectedContextTodoList : Model -> List Todo
getSelectedContextTodoList model =
    model.todoStore |> TodoStore.listForContextId model.contextId


getSelectedContextId =
    .contextId



---- UPDATE ----


andThenUpdate msg =
    andThen (update msg)


logCmd =
    Log.warn "Main"


type Msg
    = NoOp
    | OnWindowSize Int Int
    | OnFocusResult Focus.FocusResult
    | NavigateToTodoListWithContextId ContextId
    | TodoStoreMsg TodoStore.Msg
    | ContextStoreMsg ContextStore.Msg
    | MenuClicked
    | TempSidebarBackdropClicked
    | ToggleShowArchivedContexts
    | ToggleCompletedTodos
    | OpenCreateTodoDialog
    | OpenEditTodoDialog TodoId
    | OpenCreateContextDialog
    | OpenEditContextDialog ContextId
    | OpenContextPopup ContextId
    | OpenCmdDialog
    | EditSelectedTodo
    | LayerMsg Layer.Msg
    | OnKeyDown HotKey.Event
    | OnTodoFocusIn TodoId


type alias ContextItem =
    ( String, ContextId )


selectionConfig : Model -> TodoListSelection.Config Msg
selectionConfig model =
    { todoStore = model.todoStore
    , selectedContextId = model.contextId
    , selectedIndex = model.selectedIndex
    , onFocusResult = OnFocusResult
    }


getComputedSelectedIndex =
    TodoListSelection.getComputedSelectedIndex << selectionConfig


getMaybeSelectedTodo model =
    let
        ( active, completed ) =
            getSelectedContextTodoList model
                |> List.partition TodoStore.isNotDone

        total =
            List.length active
    in
    if total <= 0 then
        Nothing

    else
        active |> Array.fromList |> Array.get (getComputedSelectedIndex model)


cycleSelectedIndexBy num model =
    let
        ( active, completed ) =
            getSelectedContextTodoList model
                |> List.partition TodoStore.isNotDone

        total =
            List.length active
    in
    if total > 0 then
        let
            selectedIndex =
                safeModBy total (model.selectedIndex + num)

            focusCmd =
                Array.fromList active
                    |> Array.get selectedIndex
                    |> Maybe.map ContextTodoList.getTodoDomId
                    |> Focus.attemptMaybe OnFocusResult
        in
        ( { model | selectedIndex = selectedIndex }, focusCmd )

    else
        ( model, Cmd.none )


setSelectedIndexOnFocusIn todoId model =
    let
        ( active, completed ) =
            getSelectedContextTodoList model
                |> List.partition TodoStore.isNotDone

        selectedIndex =
            Array.fromList active
                |> Array.toIndexedList
                |> List.filter (Tuple.second >> .id >> eqs todoId)
                |> List.head
                |> unwrapMaybe model.selectedIndex Tuple.first
    in
    ( { model | selectedIndex = selectedIndex }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        OnFocusResult r ->
            model
                |> pure
                >> addCmd (Log.focusResult "Main.elm" r)

        OnTodoFocusIn todoId ->
            setSelectedIndexOnFocusIn todoId model

        OnWindowSize width height ->
            pure { model | windowSize = { width = width, height = height } }

        OnKeyDown ke ->
            case model.layer of
                Layer.NoLayer ->
                    case ke of
                        ( [], "ArrowDown" ) ->
                            cycleSelectedIndexBy 1 model

                        ( [], "ArrowUp" ) ->
                            cycleSelectedIndexBy -1 model

                        ( [], "q" ) ->
                            update OpenCreateTodoDialog model

                        ( [ Shift, Meta ], "a" ) ->
                            update OpenCmdDialog model

                        ( [], "e" ) ->
                            update EditSelectedTodo model

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NavigateToTodoListWithContextId contextId ->
            pure { model | contextId = contextId, showTempSidebar = False }

        TodoStoreMsg msg ->
            Update2.lift
                .todoStore
                (\s b -> { b | todoStore = s })
                TodoStoreMsg
                TodoStore.update
                msg
                model

        ContextStoreMsg msg ->
            Update2.lift
                .contextStore
                (\s b -> { b | contextStore = s })
                ContextStoreMsg
                ContextStore.update
                msg
                model

        OpenEditContextDialog cid ->
            updateLayer (Layer.OpenEditContextDialog cid) model

        OpenContextPopup cid ->
            updateLayer (Layer.OpenContextPopup cid) model

        LayerMsg msg ->
            updateLayer msg model

        OpenCreateTodoDialog ->
            updateLayer (Layer.OpenCreateTodoDialog <| getSelectedContextId model) model

        OpenEditTodoDialog todoId ->
            updateLayer (Layer.OpenEditTodoDialog todoId) model

        EditSelectedTodo ->
            pure model
                |> unwrapMaybe identity
                    (andThen << update << OpenEditTodoDialog << .id)
                    (getMaybeSelectedTodo model)

        OpenCreateContextDialog ->
            updateLayer Layer.OpenCreateContextDialog model

        OpenCmdDialog ->
            updateLayer Layer.OpenCmdDialog model

        MenuClicked ->
            pure { model | showTempSidebar = not model.showTempSidebar }

        TempSidebarBackdropClicked ->
            pure { model | showTempSidebar = False }

        ToggleShowArchivedContexts ->
            pure { model | showArchivedContexts = not model.showArchivedContexts }

        ToggleCompletedTodos ->
            pure { model | showCompletedTodos = not model.showCompletedTodos }


updateLayer message model =
    let
        ( layer, cmd, outMsg ) =
            Layer.update model message model.layer

        handleOut =
            case outMsg of
                Layer.TodoStoreMsg msg ->
                    andThenUpdate (TodoStoreMsg msg)

                Layer.ContextStoreMsg msg ->
                    andThenUpdate (ContextStoreMsg msg)

                Layer.NavigateToTodoListWithContextId contextId ->
                    andThenUpdate <| NavigateToTodoListWithContextId contextId

                Layer.NoOut ->
                    identity
    in
    pure { model | layer = layer }
        |> addTaggedCmd LayerMsg cmd
        |> handleOut


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <| D.map OnKeyDown HotKey.decoder
        , Layer.subscriptions model.layer |> Sub.map LayerMsg
        , Browser.Events.onResize OnWindowSize
        ]



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "flex flex-column min-h-100 w-100" ]
        [ AppBar.view { menuClicked = MenuClicked }
        , div [ class " flex-auto flex flex-row justify-center" ]
            [ sDiv [ maxWidth (px 940) ]
                [ class "flex-auto flex flex-row" ]
                [ HtmlX.when (not << .showTempSidebar) viewPermanentSidebar model
                , viewContextTodoList model
                ]
            ]
        , HtmlX.when .showTempSidebar viewDrawerSidebar model
        , Layer.viewLayer model |> Html.map LayerMsg
        ]


viewPermanentSidebar : Model -> Html Msg
viewPermanentSidebar model =
    sDiv [ minWidth (px 180) ]
        [ class "flex-shrink-0  overflow-y-scroll w-30-ns dn db-ns " ]
        [ viewSidebar model
        ]


viewDrawerSidebar model =
    sDiv [ position absolute, absFill, dFlexRow, bcBlackA 0.4 ]
        []
        [ sDiv
            [ bg "white"
            , Css.width (pct 80)
            ]
            []
            [ viewSidebar model ]
        , sDiv [ fa ] [ onClick TempSidebarBackdropClicked ] []
        ]


viewSidebar model =
    let
        config : Sidebar.Config Msg
        config =
            { contextStore = model.contextStore
            , todoStore = model.todoStore
            , addContextClicked = OpenCreateContextDialog
            , showArchived = model.showArchivedContexts
            , toggleShowArchived = ToggleShowArchivedContexts
            , isSelected = eqs <| getSelectedContextId model
            , navigateToTodoList = NavigateToTodoListWithContextId
            , moreClicked = OpenContextPopup
            , moreOpen = \id -> Layer.eqContextPopupFor id model.layer
            }
    in
    Sidebar.view config



-- TodoList Page


viewContextTodoList model =
    let
        config : TodoListConfig Msg
        config =
            { todoStore = model.todoStore
            , contextStore = model.contextStore
            , toggleShowCompleted = ToggleCompletedTodos
            , isShowingCompleted = model.showCompletedTodos
            , selectedIndex = getComputedSelectedIndex model
            , markDone = TodoStoreMsg << TodoStore.markDone
            , unmarkDone = TodoStoreMsg << TodoStore.unmarkDone
            , focusInMsg = OnTodoFocusIn
            , editMsg = OpenEditTodoDialog
            , selectedContextId = getSelectedContextId model
            , addNewMsg = OpenCreateTodoDialog
            }
    in
    ContextTodoList.view config



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.toUnstyled << view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
