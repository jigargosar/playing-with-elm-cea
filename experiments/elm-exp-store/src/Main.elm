module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import ContextStore exposing (Context, ContextId, ContextName, ContextStore)
import Dict exposing (Dict)
import FeatherIcons
import HotKey
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Json.Decode as D
import Json.Encode as E
import JsonCodecX exposing (Value)
import Log
import MagicMenu exposing (MagicMenu)
import Mode exposing (Mode)
import Port
import Process
import Random
import SelectUI
import Set exposing (Set)
import SnackBar exposing (SnackBar, SnackBarTitle)
import Svg.Attributes
import Task
import Time
import TodoStore exposing (Todo, TodoStore)
import UI exposing (..)
import Update2
import Update3
import UpdateReturn exposing (..)
import WheelEvent exposing (WheelEvent)



---- MODEL ----


type Page
    = ContextTodoList
    | ContextList


type alias Model =
    { magicMenu : MagicMenu
    , snackBar : SnackBar
    , todoStore : TodoStore
    , contextStore : ContextStore
    , contextId : ContextId
    , selectContextUI : SelectUI.Model
    , mode : Mode
    , page : Page
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
    in
    pure
        { magicMenu = MagicMenu.initial
        , snackBar = SnackBar.empty
        , todoStore = todoStore
        , contextStore = contextStore
        , contextId = ContextStore.defaultId
        , selectContextUI = SelectUI.new
        , mode = Mode.init
        , page = ContextTodoList
        }
        |> andThenUpdate (unwrapMaybe NoOp Warn maybeTodoStoreLogLine)
        |> andThenUpdate (unwrapMaybe NoOp Warn maybeContextStoreLogLine)


getSelectedContextTodoList : Model -> List Todo
getSelectedContextTodoList model =
    model.todoStore
        |> TodoStore.list
        |> List.filter (.contextId >> eqs (getSelectedContextId model))
        |> List.sortBy .createdAt


getCurrentContextList : Model -> List Context
getCurrentContextList model =
    model.contextStore
        |> ContextStore.list
        |> List.sortBy .createdAt


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



---- UPDATE ----


andThenUpdate msg =
    andThen (update msg)


type Msg
    = NoOp
    | Warn Log.Line
    | SnackBarMsg SnackBar.Msg
    | SetPage Page
    | SetContextId ContextId
    | SelectContextUIMsg (SelectUI.Msg ContextItem)
    | SwitchToContextTodoListWithContextId ContextId
    | SwitchToContextTodoList
    | TodoStoreMsg TodoStore.Msg
    | ContextStoreMsg ContextStore.Msg
    | MagicMenuMsg MagicMenu.Msg
    | ModeMsg Mode.Msg
    | ModeOutMsg Mode.OutMsg


type alias ContextItem =
    ( String, ContextId )


selectContextUIConfig : SelectUI.Config Msg ContextItem
selectContextUIConfig =
    { onSelect = Tuple.second >> SwitchToContextTodoListWithContextId
    , toMsg = SelectContextUIMsg
    , toLabel = Tuple.first
    , domId = "todo-list-header-select-context-dom-id"
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        Warn logMessages ->
            ( model, Log.warn "Main" logMessages )
                |> andThenUpdate (SnackBarMsg <| SnackBar.show ("Main: " :: logMessages |> String.join ""))

        SnackBarMsg msg ->
            Update2.lift
                .snackBar
                (\s b -> { b | snackBar = s })
                SnackBarMsg
                SnackBar.update
                msg
                model

        SetPage page ->
            pure { model | page = page }

        SetContextId contextId ->
            pure { model | contextId = contextId }

        SwitchToContextTodoListWithContextId contextId ->
            pure model
                |> andThenUpdate SwitchToContextTodoList
                |> andThenUpdate (SetContextId contextId)

        SwitchToContextTodoList ->
            pure model |> andThenUpdate (SetPage <| ContextTodoList)

        SelectContextUIMsg msg ->
            updateSub (SelectUI.update selectContextUIConfig)
                .selectContextUI
                (\s b -> { b | selectContextUI = s })
                msg
                model

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

        ModeMsg msg ->
            let
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
            in
            updateMode msg model

        ModeOutMsg msg ->
            case msg of
                Mode.AddTodoOutMsg content ->
                    if isWhitespaceOrEmptyString content then
                        pure model

                    else
                        update (TodoStoreMsg <| TodoStore.addNew content) model
                            |> andThenUpdate (SnackBarMsg <| SnackBar.show "Task Added")

                Mode.AddContextOutMsg name ->
                    if isWhitespaceOrEmptyString name then
                        pure model

                    else
                        update (ContextStoreMsg <| ContextStore.addNew name) model
                            |> andThenUpdate (SnackBarMsg <| SnackBar.show "Context Added")

                Mode.TodoContentUpdatedOutMsg id content ->
                    if isWhitespaceOrEmptyString content then
                        pure model

                    else
                        update (TodoStoreMsg <| TodoStore.setContent id content) model

                Mode.SetTodoContextOutMsg todoId contextId ->
                    update (TodoStoreMsg <| TodoStore.setContextId todoId contextId) model

                Mode.ContextNameUpdatedOutMsg id name ->
                    if isWhitespaceOrEmptyString name then
                        pure model

                    else
                        update (ContextStoreMsg <| ContextStore.setName id name) model

        MagicMenuMsg msg ->
            Update2.lift
                .magicMenu
                (\s b -> { b | magicMenu = s })
                MagicMenuMsg
                MagicMenu.update
                msg
                model



---- Subscriptions


subscriptions model =
    Sub.batch
        [ MagicMenu.subscriptions model.magicMenu |> Sub.map MagicMenuMsg
        , SelectUI.subscriptions selectContextUIConfig model.selectContextUI
        ]



---- VIEW ----


startAddingTodoMsg =
    ModeMsg Mode.startAddingTodo


startAddingContextMsg =
    ModeMsg Mode.startAddingContext


startEditingContextMsg =
    ModeMsg << Mode.startEditingContext


startEditingSelectedContextMsg =
    getMaybeSelectedContext >> unwrapMaybe NoOp startEditingContextMsg


startEditingTodoContext =
    ModeMsg << Mode.startEditingTodoContext


mockActions =
    [ FeatherIcons.home
    , FeatherIcons.twitter
    , FeatherIcons.scissors
    , FeatherIcons.edit
    , FeatherIcons.moon
    ]
        |> List.map (\icon -> MagicMenu.Action icon NoOp)
        |> (::) (MagicMenu.Action FeatherIcons.trash2 NoOp)
        |> (::) (MagicMenu.Action FeatherIcons.filePlus startAddingTodoMsg)


getAllContextsNameIdPairs =
    .contextStore
        >> ContextStore.list
        >> List.map (\c -> ( c.name, c.id ))
        >> (::) ( ContextStore.defaultName, ContextStore.defaultId )


view : Model -> Html Msg
view model =
    div [ class "w-100 vs3" ]
        [ viewAppBar
        , viewPage model
        , div [ class "w-100 flex flex-column justify-center items-center" ]
            [ MagicMenu.view mockActions MagicMenuMsg model.magicMenu ]
        , Mode.viewModal (getAllContextsNameIdPairs model) model.mode |> Html.map ModeMsg
        , SnackBar.view SnackBarMsg { actions = [] } model.snackBar
        ]


viewAppBar =
    div [ class "flex row justify-center bg-black white" ]
        [ div [ class "w-100 measure" ]
            [ div [ class "flex flex-row pa3" ] [ txtC "b" "ELM", txtC "fw3" "DONE" ] ]
        ]


viewPage model =
    let
        viewPageHeader =
            div [ class "flex row justify-center" ]
                [ div [ class "measure" ]
                    [ button [ onClick <| SetPage ContextList ] [ text "Contexts" ]

                    --                , button [ onClick <| SwitchToContextTodoList ] [ text "Tasks" ]
                    , button [ onClick startAddingContextMsg ] [ text "Add Context" ]
                    , button [ onClick startAddingTodoMsg ] [ text "Add Task" ]
                    ]
                ]

        viewPageContent =
            div [ class "flex row justify-center" ]
                [ div [ class "measure w-100 vs3" ]
                    (case model.page of
                        ContextTodoList ->
                            [ viewTodoListHeader model
                            , viewTodoList model
                            ]

                        ContextList ->
                            [ viewContextList model ]
                    )
                ]
    in
    div [ class "flex flex-row" ]
        [ div [ class "w-30" ] []
        , div [ class "flex-auto flex flex-column" ]
            [ viewPageHeader
            , viewPageContent
            ]
        ]



-- ContextList Page


viewContextList : Model -> Html Msg
viewContextList model =
    let
        inboxViewModel : ContextViewModel Msg
        inboxViewModel =
            ContextViewModel "Inbox" "Inbox" False NoOp (SwitchToContextTodoListWithContextId ContextStore.defaultId)

        viewPrimaryListKeyed =
            getCurrentContextList model
                |> List.map (createContextViewModel >> viewContext)
                |> (::) (viewContext inboxViewModel)
    in
    Html.Keyed.node "div" [] viewPrimaryListKeyed


type alias ContextViewModel msg =
    { key : String
    , name : String
    , isNameEditable : Bool
    , startEditingName : msg
    , switchToContextTodoList : msg
    }


createContextViewModel : Context -> ContextViewModel Msg
createContextViewModel context =
    ContextViewModel
        context.id
        (defaultEmptyStringTo "<empty context name>" context.name)
        True
        (startEditingContextMsg context)
        (SwitchToContextTodoListWithContextId context.id)


viewContext : ContextViewModel msg -> ( String, Html msg )
viewContext { key, name, startEditingName, isNameEditable, switchToContextTodoList } =
    ( key
    , row "pa3 w-100  bb b--light-gray"
        []
        [ txtA [ style "width" "24px" ] ""
        , button
            [ class "flex-auto pa0 ma0 tl ttu color-inherit normal underline pointer"
            , onClick switchToContextTodoList
            ]
            [ text <| "@" ++ name ]
        , boolHtml isNameEditable <| UI.fBtn FeatherIcons.edit3 startEditingName
        ]
    )



-- TodoList Page


viewTodoListHeader : Model -> Html Msg
viewTodoListHeader model =
    div
        [ class "flex flex-row" ]
        [ div [ class "flex-auto" ]
            [ SelectUI.view selectContextUIConfig
                (Just <| getCurrentContextItem model)
                (allContextItems model)
                model.selectContextUI
            ]
        , UI.fBtn FeatherIcons.plus startAddingTodoMsg
        ]


viewTodoList : Model -> Html Msg
viewTodoList model =
    getSelectedContextTodoList model
        |> List.map (viewKeyedTodo << createTodoViewModel model.contextStore)
        |> Html.Keyed.node "div" []


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
        (defaultEmptyStringTo "<empty task>" todo.content)
        todo.done
        (ContextStore.getNameOrDefaultById todo.contextId contextStore)
        (ModeMsg <| Mode.startEditingTodo todo)
        (TodoStoreMsg <| TodoStore.markDone todo.id)
        (TodoStoreMsg <| TodoStore.unmarkDone todo.id)
        (startEditingTodoContext todo)


viewKeyedTodo : TodoViewModel msg -> ( String, Html msg )
viewKeyedTodo { key, content, done, contentClicked, markDone, unmarkDone, contextName, contextClicked } =
    let
        doneIconBtn =
            if done then
                fBtnSA [ sClass "green" ] FeatherIcons.checkCircle unmarkDone

            else
                fBtnSA [ sClass "gray" ] FeatherIcons.circle markDone
    in
    ( key
    , row "pa3 bb b--light-gray"
        []
        [ div [] [ doneIconBtn ]
        , div [ class "flex-auto flex flex-column " ]
            [ div
                [ class "pointer"
                , classList [ ( "strike gray ", done ) ]
                , onClick contentClicked
                ]
                [ txt content ]
            , txtA
                [ class "ttu f7 gray pointer"
                , onClick contextClicked
                ]
                ("@" ++ contextName)
            ]
        ]
    )



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
