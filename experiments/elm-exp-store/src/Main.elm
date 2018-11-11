module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import Btn
import ContextPopup
import ContextStore exposing (Context, ContextId, ContextName, ContextStore)
import CreateTodoDialog
import Css exposing (..)
import CssAtoms exposing (fa, fgGray, p0, pl0, ptr, ttu, w100)
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
import Log
import Mode exposing (Mode)
import Port
import Random
import Set exposing (Set)
import Sidebar
import Styles exposing (..)
import Svg.Attributes
import Svg.Styled exposing (svg)
import Task
import Time
import TodoStore exposing (Todo, TodoContent, TodoId, TodoStore)
import UI exposing (..)
import Update2
import UpdateReturn exposing (..)



---- MODEL ----


type Layer
    = CreateTodoDialog CreateTodoDialog.Model
    | ContextPopup ContextId ContextPopup.Model
    | NoLayer


type alias Model =
    { todoStore : TodoStore
    , contextStore : ContextStore
    , contextId : ContextId
    , mode : Mode
    , layer : Layer
    , showArchivedContexts : Bool
    , showCompletedTodos : Bool
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
            , mode = Mode.init
            , layer = NoLayer
            , showArchivedContexts = False
            , showCompletedTodos = False
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


isContextPopupOpenFor cid_ model =
    case model.layer of
        ContextPopup cid contextPopup ->
            cid_ == cid

        _ ->
            False


getMaybeContext cid =
    .contextStore >> ContextStore.get cid



--maybeContextPopup
---- UPDATE ----


andThenUpdate msg =
    andThen (update msg)


type Msg
    = NoOp
    | Warn Log.Line
    | SetTodoListContextId ContextId
    | MsgTodoStore TodoStore.Msg
    | MsgContextStore ContextStore.Msg
    | MsgContextPopup ContextPopup.Msg
    | MsgCreateTodoDialog CreateTodoDialog.Msg
    | MsgMode Mode.Msg
    | StartEditingContext ContextId
    | ContextMoreClicked ContextId
    | ToggleShowArchivedContexts
    | ToggleCompletedTodos
    | ChangeLayerToCreateTodoDialog


type alias ContextItem =
    ( String, ContextId )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        Warn logMessages ->
            ( model, Log.warn "Main" logMessages )

        SetTodoListContextId contextId ->
            pure { model | contextId = contextId }

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

        MsgMode msg ->
            let
                modeUpdateConfig : Mode.UpdateConfig Msg
                modeUpdateConfig =
                    { toMsg = MsgMode
                    , addTodo = \content -> MsgTodoStore <| TodoStore.addNew content
                    , setTodoContext = \todoId contextId -> MsgTodoStore <| TodoStore.setContextId todoId contextId
                    , setTodoContent = \id content -> MsgTodoStore <| TodoStore.setContent id content
                    , addContext = MsgContextStore << ContextStore.addNew
                    , setContextName = \id name -> MsgContextStore <| ContextStore.setName id name
                    }
            in
            updateSub (Mode.update modeUpdateConfig)
                .mode
                (\s b -> { b | mode = s })
                msg
                model

        StartEditingContext cid ->
            pure model
                |> (model.contextStore
                        |> ContextStore.get cid
                        |> unwrapMaybe identity (andThenUpdate << MsgMode << Mode.startEditingContext)
                   )

        ContextMoreClicked cid ->
            pure
                { model
                    | layer = ContextPopup cid ContextPopup.init
                }
                |> andThenUpdate (MsgContextPopup ContextPopup.open)

        ChangeLayerToCreateTodoDialog ->
            case model.layer of
                NoLayer ->
                    pure
                        { model
                            | layer = CreateTodoDialog CreateTodoDialog.init
                        }
                        |> andThenUpdate (MsgCreateTodoDialog CreateTodoDialog.autoFocus)

                _ ->
                    Debug.todo "handle: replacing layer without closing it."

        MsgContextPopup msg ->
            case model.layer of
                ContextPopup cid layerModel ->
                    let
                        ( contextPopup, cmd, maybeOut ) =
                            ContextPopup.update cid msg layerModel
                    in
                    maybeOut
                        |> unwrapMaybe
                            (pure { model | layer = ContextPopup cid contextPopup })
                            (\out ->
                                ( { model | layer = NoLayer }
                                , case out of
                                    ContextPopup.ActionOut action ->
                                        msgToCmd <|
                                            case action of
                                                ContextPopup.Rename ->
                                                    StartEditingContext cid

                                                ContextPopup.ToggleArchive ->
                                                    MsgContextStore <| ContextStore.toggleArchived cid

                                    ContextPopup.ClosedOut ->
                                        Cmd.none
                                )
                            )
                        |> addTaggedCmd MsgContextPopup cmd

                _ ->
                    pure model

        MsgCreateTodoDialog msg ->
            case model.layer of
                CreateTodoDialog layerModel ->
                    let
                        ( addTodoDialogModel, cmd, maybeOutMsg ) =
                            CreateTodoDialog.update msg layerModel
                    in
                    maybeOutMsg
                        |> unwrapMaybe
                            (pure
                                { model | layer = CreateTodoDialog addTodoDialogModel }
                            )
                            (\out ->
                                ( { model | layer = NoLayer }
                                , case out of
                                    CreateTodoDialog.Submit content contextId ->
                                        msgToCmd <| MsgTodoStore (TodoStore.addNew content)

                                    CreateTodoDialog.Cancel ->
                                        Cmd.none
                                )
                            )
                        |> addTaggedCmd MsgCreateTodoDialog cmd

                _ ->
                    pure model

        ToggleShowArchivedContexts ->
            pure { model | showArchivedContexts = not model.showArchivedContexts }

        ToggleCompletedTodos ->
            pure { model | showCompletedTodos = not model.showCompletedTodos }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



---- VIEW ----


startAddingTodoMsg =
    ChangeLayerToCreateTodoDialog


startAddingContextMsg =
    MsgMode Mode.startAddingContext


startEditingTodoContext =
    MsgMode << Mode.startEditingTodoContext


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
        [ viewAppBar
        , viewPage model
        , viewLayer model
        , Mode.viewModal (getAllContextsNameIdPairs model) model.mode |> Html.map MsgMode
        ]


viewLayer model =
    case model.layer of
        ContextPopup cid contextPopup ->
            getMaybeContext cid model
                |> unwrapMaybe noHtml
                    (\c -> ContextPopup.view c contextPopup |> Html.map MsgContextPopup)

        CreateTodoDialog dialogModel ->
            CreateTodoDialog.view dialogModel |> Html.map MsgCreateTodoDialog

        NoLayer ->
            noHtml


getMaybeContextPopup model =
    List.head model.layers
        |> Maybe.andThen
            (\layer ->
                case layer of
                    ContextPopup cid contextPopup ->
                        Just ( cid, contextPopup )

                    _ ->
                        Nothing
            )


viewAppBar =
    appBar []
        [ section1 [ class "pa3" ]
            [ sDiv [ fwb ] [] [ text "ELM" ], sDiv [ fontWeight lighter ] [] [ text "DONE" ] ]
        ]


viewPage model =
    let
        viewPageContent =
            div [ class "flex row justify-center" ]
                [ div [ class "measure w-100" ]
                    [ viewTodoListHeader model
                    , viewTodoList model
                    ]
                ]
    in
    div [ class " flex-auto flex flex-row" ]
        [ div [ class "flex-shrink-0 overflow-y-scroll w-30-ns dn db-ns " ]
            [ Sidebar.view <| createSideBarConfig model
            ]
        , div [ id "popper-container", class "flex-auto  overflow-y-scroll  pv3 flex flex-column vs3" ]
            [ viewPageContent
            ]
        ]


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
            , navigateToTodoList = SetTodoListContextId id
            , activeTodoCount = getActiveTodoListCountForContextId id model
            , isSelected = isCurrentPageContextTodoListWithContextId id model
            , moreClicked = contextMoreClicked id
            , moreOpen = isContextPopupOpenFor id model
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
    div
        [ class "ph3 flex flex-row" ]
        [ div [ class "flex-auto" ]
            [{- SelectUI.view selectContextUIConfig
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
        (MsgMode <| Mode.startEditingTodo todo)
        (MsgTodoStore <| TodoStore.markDone todo.id)
        (MsgTodoStore <| TodoStore.unmarkDone todo.id)
        (startEditingTodoContext todo)


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
        [ sDiv [ hs ] [] [ doneIconBtn ]
        , sDiv [ hs ]
            [ class "flex-auto flex flex-column " ]
            [ div
                [ class "pointer"
                , classList [ ( "strike gray ", done ) ]
                , onClick contentClicked
                ]
                [ div [] [ text content ] ]
            , div
                [ class "ttu f7 gray pointer"
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
