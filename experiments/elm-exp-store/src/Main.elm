module Main exposing (main)

import AppBar
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
import FeatherIcons as Icon
import HotKey
import Html.Styled as Html exposing (Attribute, Html, button, div, fromUnstyled, span, styled, text)
import Html.Styled.Attributes exposing (class, classList, css, id, style)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as HKeyed exposing (node)
import HtmlX
import Icons
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
            }
    in
    pure model
        |> andThenUpdate (unwrapMaybe NoOp Warn maybeTodoStoreLogLine)
        |> andThenUpdate (unwrapMaybe NoOp Warn maybeContextStoreLogLine)


isCurrentPageContextTodoListWithContextId contextId model =
    getSelectedContextId model == contextId


type alias Pred a =
    a -> Bool


type alias PredList a =
    List (Pred a)


notPred : Pred a -> Pred a
notPred pred =
    pred >> not


type alias Getter big small =
    big -> small


propEq : Getter big small -> small -> Pred big
propEq getter small =
    getter >> eqs small


contextIdEq : ContextId -> Pred { a | contextId : ContextId }
contextIdEq cid =
    propEq .contextId cid


isDone : Pred { a | done : Bool }
isDone =
    propEq .done True


isNotDone =
    isDone >> not


allPass : PredList a -> Pred a
allPass plist a =
    List.all (applyTo a) plist


getSelectedContextActiveTodoList : Model -> List Todo
getSelectedContextActiveTodoList model =
    getActiveTodoListForContextId model.contextId model


getSelectedContextTodoList : Model -> List Todo
getSelectedContextTodoList model =
    getTodoListForContextId model.contextId model


getActiveTodoListCountForContextId : ContextId -> Model -> Int
getActiveTodoListCountForContextId cid =
    getActiveTodoListForContextId cid >> List.length


getActiveTodoListForContextId : ContextId -> Model -> List Todo
getActiveTodoListForContextId cid =
    .todoStore >> TodoStore.list >> List.filter (allPass [ contextIdEq cid, isNotDone ])


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


allContextItems : Model -> List ContextItem
allContextItems model =
    ContextStore.nameDict model.contextStore
        |> Dict.toList
        |> List.map swap


isSelectedContextEditable =
    getSelectedContextId >> eqs ContextStore.defaultId >> not


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


type Msg
    = NoOp
    | Warn Log.Line
    | NavigateToTodoListWithContextId ContextId
    | MsgTodoStore TodoStore.Msg
    | MsgContextStore ContextStore.Msg
    | MsgContextPopup ContextPopup.Msg
    | MsgTodoDialog TodoDialog.Msg
    | MsgContextDialog ContextDialog.Msg
    | StartEditingContext ContextId
    | ContextMoreClicked ContextId
    | MenuClicked
    | TempSidebarBackdropClicked
    | ToggleShowArchivedContexts
    | ToggleCompletedTodos
    | SwitchLayerToCreateTodoDialog
    | SwitchLayerToEditTodoDialog Todo
    | SwitchLayerToCreateContextDialog
    | SwitchLayerToEditContextDialog Context


type alias ContextItem =
    ( String, ContextId )


setLayer layer model =
    { model | layer = layer }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        Warn logMessages ->
            ( model, Log.warn "Main" logMessages )

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

        StartEditingContext cid ->
            pure model
                |> (model.contextStore
                        |> ContextStore.get cid
                        |> unwrapMaybe identity (andThenUpdate << SwitchLayerToEditContextDialog)
                   )

        ContextMoreClicked cid ->
            Layer.initMaybeContextPopup cid model.contextStore
                |> unwrapMaybe (pure model)
                    (\layer ->
                        pure { model | layer = layer }
                            |> andThenUpdate (MsgContextPopup ContextPopup.open)
                    )

        MenuClicked ->
            pure { model | showTempSidebar = not model.showTempSidebar }

        TempSidebarBackdropClicked ->
            pure { model | showTempSidebar = False }

        SwitchLayerToCreateTodoDialog ->
            case model.layer of
                Layer.NoLayer ->
                    pure
                        { model
                            | layer = Layer.TodoDialog (TodoDialog.initCreate <| getSelectedContextId model)
                        }
                        |> andThenUpdate (MsgTodoDialog TodoDialog.autoFocus)

                _ ->
                    pure model |> andThenUpdate (Warn [ "handle: replacing layer without closing it." ])

        SwitchLayerToEditTodoDialog todo ->
            case model.layer of
                Layer.NoLayer ->
                    pure
                        { model
                            | layer = Layer.TodoDialog (TodoDialog.initEdit todo)
                        }
                        |> andThenUpdate (MsgTodoDialog TodoDialog.autoFocus)

                _ ->
                    pure model |> andThenUpdate (Warn [ "handle: replacing layer without closing it." ])

        SwitchLayerToCreateContextDialog ->
            case model.layer of
                Layer.NoLayer ->
                    updateMsgContextDialog
                        ContextDialog.autoFocus
                        ContextDialog.initCreate
                        model

                _ ->
                    pure model |> andThenUpdate (Warn [ "handle: replacing layer without closing it." ])

        SwitchLayerToEditContextDialog context ->
            case model.layer of
                Layer.NoLayer ->
                    pure
                        { model
                            | layer = Layer.ContextDialog (ContextDialog.initEdit context)
                        }
                        |> andThenUpdate (MsgContextDialog ContextDialog.autoFocus)

                _ ->
                    pure model |> andThenUpdate (Warn [ "handle: replacing layer without closing it." ])

        MsgContextPopup msg ->
            case model.layer of
                Layer.ContextPopup context layerModel ->
                    let
                        ( contextPopup, cmd, maybeOut ) =
                            ContextPopup.update context.id msg layerModel
                    in
                    maybeOut
                        |> unwrapMaybe
                            (pure { model | layer = Layer.ContextPopup context contextPopup })
                            (\out ->
                                ( { model | layer = Layer.NoLayer }
                                , case out of
                                    ContextPopup.ActionOut action ->
                                        msgToCmd <|
                                            case action of
                                                ContextPopup.Rename ->
                                                    StartEditingContext context.id

                                                ContextPopup.ToggleArchive ->
                                                    MsgContextStore <| ContextStore.toggleArchived context.id

                                    ContextPopup.ClosedOut ->
                                        Cmd.none
                                )
                            )
                        |> addTaggedCmd MsgContextPopup cmd

                _ ->
                    pure model

        MsgTodoDialog msg ->
            case model.layer of
                Layer.TodoDialog layerModel ->
                    let
                        ( editTodoDialogModel, cmd, maybeOutMsg ) =
                            TodoDialog.update msg layerModel
                    in
                    maybeOutMsg
                        |> unwrapMaybe
                            (pure
                                { model | layer = Layer.TodoDialog editTodoDialogModel }
                            )
                            (\out ->
                                ( { model | layer = Layer.NoLayer }
                                , case out of
                                    TodoDialog.Submit dialogMode content contextId ->
                                        msgToCmd <|
                                            MsgTodoStore <|
                                                case dialogMode of
                                                    TodoDialog.Create ->
                                                        TodoStore.addNew content contextId

                                                    TodoDialog.Edit todo ->
                                                        TodoStore.setContentAndContextId todo.id content contextId

                                    TodoDialog.Cancel ->
                                        Cmd.none
                                )
                            )
                        |> addTaggedCmd MsgTodoDialog cmd

                _ ->
                    pure model

        MsgContextDialog msg ->
            case model.layer of
                Layer.ContextDialog contextDialog ->
                    updateMsgContextDialog msg contextDialog model

                _ ->
                    pure model

        ToggleShowArchivedContexts ->
            pure { model | showArchivedContexts = not model.showArchivedContexts }

        ToggleCompletedTodos ->
            pure { model | showCompletedTodos = not model.showCompletedTodos }


updateMsgContextDialog msg contextDialog_ model =
    let
        ( contextDialog, cmd, maybeOutMsg ) =
            ContextDialog.update msg contextDialog_
    in
    maybeOutMsg
        |> unwrapMaybe
            (pure
                { model | layer = Layer.ContextDialog contextDialog }
            )
            (\out ->
                ( { model | layer = Layer.NoLayer }
                , case out of
                    ContextDialog.Submit dialogMode name ->
                        msgToCmd <|
                            MsgContextStore <|
                                case dialogMode of
                                    ContextDialog.Create ->
                                        ContextStore.addNew name

                                    ContextDialog.Edit context ->
                                        ContextStore.setName context.id name

                    ContextDialog.Cancel ->
                        Cmd.none
                )
            )
        |> addTaggedCmd MsgContextDialog cmd


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



---- VIEW ----


startAddingTodoMsg =
    SwitchLayerToCreateTodoDialog


startAddingContextMsg =
    SwitchLayerToCreateContextDialog


startEditingTodoContext =
    SwitchLayerToEditContextDialog


contextMoreClicked =
    ContextMoreClicked


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
                [ HtmlX.when (not << .showTempSidebar) permanentSidebar model
                , div [ class "bl br b--black-05 flex-auto  overflow-y-scroll  pv3 flex flex-column vs3" ]
                    [ viewTodoListHeader model
                    , viewTodoList model
                    ]
                ]
            ]

        --        , QuickAction.view
        , HtmlX.when .showTempSidebar viewTempSidebar model
        , viewLayer model
        ]


permanentSidebar model =
    sDiv [ minWidth (px 180) ]
        [ class "flex-shrink-0  overflow-y-scroll w-30-ns dn db-ns " ]
        [ Sidebar.view <| createSideBarConfig model
        ]


viewTempSidebar model =
    sDiv [ position absolute, absFill, dFlexRow, bcBlackA 0.4 ]
        []
        [ sDiv
            [ {- position absolute
                 , left zero
                 , top zero
                 , height (vh 100)
                 ,
              -}
              bg "white"
            , Css.width (pct 80)
            ]
            []
            [ Sidebar.view <| createSideBarConfig model ]
        , sDiv [ fa ] [ onClick TempSidebarBackdropClicked ] []
        ]


viewLayer model =
    case model.layer of
        Layer.ContextPopup context contextPopup ->
            ContextPopup.view context contextPopup |> Html.map MsgContextPopup

        Layer.TodoDialog dialogModel ->
            TodoDialog.view model.contextStore dialogModel |> Html.map MsgTodoDialog

        Layer.ContextDialog dialogModel ->
            ContextDialog.view dialogModel |> Html.map MsgContextDialog

        Layer.NoLayer ->
            noHtml


createSideBarConfig : Model -> Sidebar.Config Msg
createSideBarConfig model =
    let
        createContextConfig : ContextId -> ContextName -> Bool -> Sidebar.ContextConfig Msg
        createContextConfig id name isArchived =
            { key = id
            , id = id
            , cid = id
            , name = name
            , isArchived = isArchived
            , navigateToTodoList = NavigateToTodoListWithContextId id
            , activeTodoCount = getActiveTodoListCountForContextId id model
            , isSelected = isCurrentPageContextTodoListWithContextId id model
            , moreClicked = contextMoreClicked id
            , moreOpen = Layer.eqContextPopupFor id model.layer
            }

        contexts =
            getUserDefinedContextList model
                |> List.map (\c -> createContextConfig c.id c.name c.archived)

        inbox =
            createContextConfig ContextStore.defaultId ContextStore.defaultName False
    in
    { inbox = inbox
    , contexts = contexts
    , addContextClicked = startAddingContextMsg
    , showArchived = model.showArchivedContexts
    , toggleShowArchived = ToggleShowArchivedContexts
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
            [ text name

            {- SelectUI.view selectContextUIConfig
               (Just <| getCurrentContextItem model)
               (allContextItems model)
               model.selectContextUI
            -}
            ]
        ]


viewTodoList : Model -> Html Msg
viewTodoList model =
    let
        ( active, completed ) =
            getSelectedContextTodoList model
                |> List.partition isNotDone
    in
    div [ css [] ]
        [ active
            |> List.map (viewKeyedTodo << createTodoViewModel model.contextStore)
            |> HKeyed.node "div" [ css [ vs ] ]
        , div [ css [ rowCY ], class "pa3" ]
            [ styled Btn.flatPl0
                [ fontSize (rem 0.8), fa ]
                [ onClick startAddingTodoMsg ]
                [ Icons.plusSmall
                , text "Add Task"
                ]
            , viewCompletedBtn model.showCompletedTodos
            ]
        , if model.showCompletedTodos then
            viewCompletedSection model.contextStore completed

          else
            noHtml
        ]


viewCompletedSection contextStore completed =
    if List.isEmpty completed then
        sDiv [ fgGray ] [ class "pa3" ] [ text "No Completed Tasks" ]

    else
        completed
            |> List.map (viewKeyedTodo << createTodoViewModel contextStore)
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
    }


createTodoViewModel : ContextStore -> Todo -> TodoViewModel Msg
createTodoViewModel contextStore todo =
    TodoViewModel
        todo.id
        (defaultEmptyStringTo "<empty>" todo.content)
        todo.done
        (ContextStore.getNameOrDefaultById todo.contextId contextStore)
        (SwitchLayerToEditTodoDialog todo)
        (MsgTodoStore <| TodoStore.markDone todo.id)
        (MsgTodoStore <| TodoStore.unmarkDone todo.id)
        (SwitchLayerToEditTodoDialog todo)


viewKeyedTodo : TodoViewModel msg -> ( String, Html msg )
viewKeyedTodo { key, content, done, contentClicked, markDone, unmarkDone, contextName, contextClicked } =
    let
        doneIconBtn =
            if done then
                Btn.sIcon [ fg "green" ] [ onClick unmarkDone ] [ Icons.checkCircle |> Icons.default ]

            else
                Btn.sIcon [ fg "gray" ] [ onClick markDone ] [ Icons.circle |> Icons.default ]
    in
    ( key
    , sDiv [ rowCY ]
        [ class "pa3 bb b--light-gray" ]
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
