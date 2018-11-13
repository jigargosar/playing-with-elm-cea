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
import Css exposing (..)
import Dict exposing (Dict)
import DomX exposing (DomId)
import FeatherIcons as Icon
import Focus
import HotKey exposing (SoftKey(..))
import Html.Styled as Html exposing (Attribute, Html, button, div, fromUnstyled, span, styled, text)
import Html.Styled.Attributes exposing (class, classList, css, id, style, tabindex)
import Html.Styled.Events exposing (onClick)
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
    }


type alias Flags =
    { now : Millis, todos : Value, contexts : Value }


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
            }
    in
    ( model
    , [ maybeTodoStoreLogLine, maybeContextStoreLogLine ]
        |> List.filterMap (Maybe.map logCmd)
        |> Cmd.batch
    )


contextIdEq : ContextId -> Pred { a | contextId : ContextId }
contextIdEq cid =
    propEq .contextId cid


getSelectedContextTodoList : Model -> List Todo
getSelectedContextTodoList model =
    getTodoListForContextId model.contextId model


getActiveTodoListCountForContextId : ContextId -> Model -> Int
getActiveTodoListCountForContextId cid =
    getActiveTodoListForContextId cid >> List.length


getActiveTodoListForContextId : ContextId -> Model -> List Todo
getActiveTodoListForContextId cid =
    .todoStore >> TodoStore.list >> List.filter (allPass [ contextIdEq cid, TodoStore.isNotDone ])


getTodoListForContextId : ContextId -> Model -> List Todo
getTodoListForContextId cid =
    .todoStore >> TodoStore.list >> List.filter (allPass [ contextIdEq cid ])


getUserDefinedContextList : Model -> List Context
getUserDefinedContextList model =
    model.contextStore
        |> ContextStore.list
        |> List.sortBy .createdAt


getNameByContextId : ContextId -> Model -> ContextName
getNameByContextId contextId =
    .contextStore >> ContextStore.getNameOrDefaultById contextId


getCurrentContextItem : Model -> ContextItem
getCurrentContextItem model =
    ( getNameByContextId model.contextId model, model.contextId )


getSelectedContextId =
    .contextId


getMaybeSelectedContext model =
    model.contextStore |> ContextStore.get (getSelectedContextId model)


getMaybeContext cid =
    .contextStore >> ContextStore.get cid



--maybeContextPopup
---- UPDATE ----


andThenUpdate msg =
    andThen (update msg)


logCmd =
    Log.warn "Main"


type Msg
    = NoOp
    | FocusResult Focus.FocusResult
    | NavigateToTodoListWithContextId ContextId
    | MsgTodoStore TodoStore.Msg
    | MsgContextStore ContextStore.Msg
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
    | LayerMsg Layer.Msg
    | OnKeyDown HotKey.Event
    | OnTodoFocusIn Todo


type alias ContextItem =
    ( String, ContextId )


getComputedSelectedIndex model =
    let
        ( active, completed ) =
            getSelectedContextTodoList model
                |> List.partition TodoStore.isNotDone

        total =
            List.length active
    in
    min (total - 1) model.selectedIndex


getTodoDomId todo =
    "todo-list-item" ++ todo.id


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
                safeModMy total (model.selectedIndex + num)

            focusCmd =
                Array.fromList active
                    |> Array.get selectedIndex
                    |> Maybe.map getTodoDomId
                    |> Focus.attemptMaybe FocusResult
        in
        ( { model | selectedIndex = selectedIndex }, focusCmd )

    else
        ( model, Cmd.none )


setSelectedIndexOnFocusIn todo model =
    let
        ( active, completed ) =
            getSelectedContextTodoList model
                |> List.partition TodoStore.isNotDone

        selectedIndex =
            Array.fromList active
                |> Array.toIndexedList
                |> List.filter (Tuple.second >> .id >> eqs todo.id)
                |> List.head
                |> unwrapMaybe model.selectedIndex Tuple.first
    in
    ( { model | selectedIndex = selectedIndex }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        FocusResult r ->
            model
                |> pure
                >> addCmd (Log.focusResult "Main.elm" r)

        OnTodoFocusIn todo ->
            setSelectedIndexOnFocusIn todo model

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

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NavigateToTodoListWithContextId contextId ->
            pure { model | contextId = contextId, showTempSidebar = False }

        MsgTodoStore msg ->
            Update2.lift
                .todoStore
                (\s b -> { b | todoStore = s })
                MsgTodoStore
                TodoStore.update
                msg
                model

        MsgContextStore msg ->
            Update2.lift
                .contextStore
                (\s b -> { b | contextStore = s })
                MsgContextStore
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
                    andThenUpdate (MsgTodoStore msg)

                Layer.ContextStoreMsg msg ->
                    andThenUpdate (MsgContextStore msg)

                Layer.NoOut ->
                    identity
    in
    pure { model | layer = layer }
        |> addTaggedCmd LayerMsg cmd
        |> handleOut


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown <| D.map OnKeyDown HotKey.decoder ]



---- VIEW ----


getAllContextsNameIdPairs =
    .contextStore
        >> ContextStore.list
        >> List.map (\c -> ( c.name, c.id ))
        >> (::) ( ContextStore.defaultName, ContextStore.defaultId )


view : Model -> Html Msg
view model =
    div [ class "flex flex-column min-h-100 w-100" ]
        [ AppBar.view { menuClicked = MenuClicked }
        , div [ class " flex-auto flex flex-row justify-center" ]
            [ sDiv [ maxWidth (px 940) ]
                [ class "flex-auto flex flex-row" ]
                [ HtmlX.when (not << .showTempSidebar) viewPermanentSidebar model
                , div [ class "bl br b--black-05 flex-auto  overflow-y-scroll  pv3 flex flex-column vs3" ]
                    [ viewTodoListHeader model
                    , viewTodoList model
                    ]
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


viewSidebar =
    Sidebar.view << createSideBarConfig


createSideBarConfig : Model -> Sidebar.Config Msg
createSideBarConfig model =
    { contexts = getUserDefinedContextList model
    , addContextClicked = OpenCreateContextDialog
    , showArchived = model.showArchivedContexts
    , toggleShowArchived = ToggleShowArchivedContexts
    , isSelected = \id -> contextIdEq id model
    , navigateToTodoList = NavigateToTodoListWithContextId
    , activeTodoCount = \id -> getActiveTodoListCountForContextId id model
    , moreClicked = OpenContextPopup
    , moreOpen = \id -> Layer.eqContextPopupFor id model.layer
    }



-- TodoList Page


viewTodoListHeader : Model -> Html Msg
viewTodoListHeader model =
    let
        ( name, contextId ) =
            getCurrentContextItem model
    in
    div
        [ class "ph3 flex flex-row" ]
        [ div [ class "flex-auto" ]
            [ text name ]
        ]


viewTodoList : Model -> Html Msg
viewTodoList model =
    let
        ( active, completed ) =
            getSelectedContextTodoList model
                |> List.partition TodoStore.isNotDone
    in
    div [ css [] ]
        [ active
            |> List.indexedMap (\idx todo -> viewKeyedTodo (createTodoViewModel model idx todo))
            |> HKeyed.node "div" [ css [ vs ] ]
        , div [ css [ rowCY ], class "pa3" ]
            [ styled Btn.flatPl0
                [ fontSize (rem 0.8), fa ]
                [ onClick OpenCreateTodoDialog ]
                [ Icons.plusSmall
                , text "Add Task"
                ]
            , viewCompletedBtn model.showCompletedTodos
            ]
        , if model.showCompletedTodos then
            viewCompletedSection model completed

          else
            noHtml
        ]


viewCompletedSection model completed =
    if List.isEmpty completed then
        sDiv [ fgGray ] [ class "pa3" ] [ text "No Completed Tasks" ]

    else
        completed
            |> List.indexedMap (\idx todo -> viewKeyedTodo (createTodoViewModel model idx todo))
            |> HKeyed.node "div" [ css [ vs ] ]


viewCompletedBtn showCompletedTodos =
    styled Btn.flatPr0
        [ fontSize (rem 0.8) ]
        [ onClick ToggleCompletedTodos ]
        [ sDiv [ rowCXY, hs ] [] [ text "Completed" ]
        , sDiv [ rowCXY, hs ]
            []
            [ if showCompletedTodos then
                Icons.toggleRightDef

              else
                Icons.toggleLeftDef
            ]
        ]


type alias TodoViewModel msg =
    { key : String
    , content : String
    , done : Bool
    , contextName : String
    , contentClicked : msg
    , markDone : msg
    , unmarkDone : msg
    , contextClicked : msg
    , isSelected : Bool
    , domId : DomId
    , focusInMsg : msg
    }


createTodoViewModel : Model -> Int -> Todo -> TodoViewModel Msg
createTodoViewModel model idx todo =
    TodoViewModel
        todo.id
        (defaultEmptyStringTo "<empty>" todo.content)
        todo.done
        (ContextStore.getNameOrDefaultById todo.contextId model.contextStore)
        (OpenEditTodoDialog todo.id)
        (MsgTodoStore <| TodoStore.markDone todo.id)
        (MsgTodoStore <| TodoStore.unmarkDone todo.id)
        (OpenEditTodoDialog todo.id)
        (idx == getComputedSelectedIndex model)
        (getTodoDomId todo)
        (OnTodoFocusIn todo)


viewKeyedTodo : TodoViewModel msg -> ( String, Html msg )
viewKeyedTodo { key, content, done, contentClicked, markDone, unmarkDone, contextName, contextClicked, isSelected, domId, focusInMsg } =
    let
        doneIconBtn =
            if done then
                Btn.sIcon [ fg "green" ] [ onClick unmarkDone ] [ Icons.checkCircle |> Icons.default ]

            else
                Btn.sIcon [ fg "gray" ] [ onClick markDone ] [ Icons.circle |> Icons.default ]
    in
    ( key
    , sDiv
        [ rowCY
        , if isSelected then
            bg "lightblue"

          else
            bg "transparent"
        ]
        [ id domId, class "pa3 bb b--light-gray", tabindex 0, DomX.onFocusIn focusInMsg ]
        [ sDiv [ hs, rowCY ] [] [ doneIconBtn ]
        , sDiv [ hs ]
            [ class "flex-auto flex flex-column " ]
            [ div
                [ class "pointer"
                , classList [ ( "strike gray ", done ) ]
                , onClick contentClicked
                ]
                [ sDiv [ Css.property "word-break" "break-word" ] [] [ text content ] ]
            , div
                [ class "ttu f7 gray pointer dn"
                , onClick contextClicked
                ]
                [ text <| "@" ++ contextName ]
            ]
        ]
    )



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = Html.toUnstyled << view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
