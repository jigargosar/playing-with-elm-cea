module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import Btn
import ContextStore exposing (Context, ContextId, ContextName, ContextStore)
import ContextType
import Css exposing (Color, backgroundColor, color, em, fontWeight, hex, hover, hsla, margin2, normal, num, padding2, paddingLeft, rgb, textDecoration, underline, zero)
import Css.Global exposing (global)
import CssAtoms exposing (fa, pl0, ptr, ttu)
import Dict exposing (Dict)
import FeatherIcons as Icon
import HotKey
import Html.Styled as Html exposing (Attribute, Html, button, div, fromUnstyled, span, styled, text)
import Html.Styled.Attributes exposing (class, classList, css, style)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as HKeyed exposing (node)
import Icons
import JsonCodecX exposing (Value)
import Log
import MagicMenu exposing (MagicMenu)
import Mode exposing (Mode)
import Port
import Random
import SelectUI
import Set exposing (Set)
import SnackBar exposing (SnackBar, SnackBarTitle)
import Styles exposing (..)
import Svg.Attributes
import Svg.Styled exposing (svg)
import Task
import Time
import TodoStore exposing (Todo, TodoStore)
import UI exposing (..)
import UI.Icon
import Update2
import UpdateReturn exposing (..)



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


isCurrentPageContextTodoListWithContextId contextId model =
    model.page == ContextTodoList && getSelectedContextId model == contextId


isCurrentPageContextList model =
    model.page == ContextList


getSelectedContextTodoList : Model -> List Todo
getSelectedContextTodoList model =
    model.todoStore
        |> TodoStore.list
        |> List.filter (.contextId >> eqs (getSelectedContextId model))
        |> List.sortBy .createdAt


getActiveTodoListCountForContextId : ContextId -> Model -> Int
getActiveTodoListCountForContextId cid =
    .todoStore >> TodoStore.list >> List.filter (.contextId >> eqs cid) >> List.length


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


navigateToInbox =
    SwitchToContextTodoListWithContextId ContextStore.defaultId


mockActions =
    [ Icon.home
    , Icon.twitter
    , Icon.scissors
    , Icon.edit
    , Icon.moon
    ]
        |> List.map (\icon -> MagicMenu.Action icon NoOp)
        |> (::) (MagicMenu.Action Icon.trash2 NoOp)
        |> (::) (MagicMenu.Action Icon.filePlus startAddingTodoMsg)


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
        , MagicMenu.view mockActions MagicMenuMsg model.magicMenu
        , Mode.viewModal (getAllContextsNameIdPairs model) model.mode |> Html.map ModeMsg
        , SnackBar.view SnackBarMsg { actions = [] } model.snackBar
        ]


viewAppBar =
    div [ class "flex-shrink-0 flex flex-row justify-center bg-black white shadow-1" ]
        [ div [ class "w-100 measure" ]
            [ div [ class "flex flex-row pa3" ] [ txtC "b" "ELM", txtC "fw3" "DONE" ] ]
        ]


viewPage model =
    let
        viewPageHeader =
            div [ class "flex row justify-center" ]
                [ div [ class "measure" ]
                    [--                    button [ onClick <| SetPage ContextList ] [ text "Contexts" ]
                     --                , button [ onClick <| SwitchToContextTodoList ] [ text "Tasks" ]
                     --                    , button [ onClick startAddingContextMsg ] [ text "Add Context" ]
                     --                      button [ onClick startAddingTodoMsg ] [ text "Add Task" ]
                    ]
                ]

        viewPageContent =
            div [ class "flex row justify-center" ]
                [ div [ class "measure w-100" ]
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
    div [ class " flex-auto flex flex-row" ]
        [ div [ class "flex-shrink-0 overflow-y-scroll w-30-ns dn db-ns " ]
            [ viewSidebar model
            ]
        , div [ class "flex-auto  overflow-y-scroll  pv3 flex flex-column vs3" ]
            [ viewPageContent
            ]
        ]


type alias ContextItemViewModel msg =
    { key : String
    , id : ContextId
    , name : ContextName
    , navigateToTodoList : msg
    , activeTodoCount : Int
    , isSelected : Bool
    }


createUserDefinedContextItemViewModel : Model -> List (ContextItemViewModel Msg)
createUserDefinedContextItemViewModel model =
    getUserDefinedContextList model
        |> List.map
            (\c ->
                { key = c.id
                , id = c.id
                , name = c.name
                , navigateToTodoList =
                    SwitchToContextTodoListWithContextId c.id
                , activeTodoCount = getActiveTodoListCountForContextId c.id model
                , isSelected = isCurrentPageContextTodoListWithContextId c.id model
                }
            )


createInboxContextItemViewModel : Model -> ContextItemViewModel Msg
createInboxContextItemViewModel model =
    { key = ContextStore.defaultId
    , id = ContextStore.defaultId
    , name = ContextStore.defaultName
    , navigateToTodoList = navigateToInbox
    , activeTodoCount = getActiveTodoListCountForContextId ContextStore.defaultId model
    , isSelected = isCurrentPageContextTodoListWithContextId ContextStore.defaultId model
    }


viewSidebar model =
    let
        badge =
            let
                sizeVal =
                    em 1.4
            in
            styled div
                [ Css.borderRadius (pct 50)
                , Css.backgroundColor <| Css.hsla 190 1 0.5 0.6
                , Css.fontSize (px 14)
                , Css.lineHeight sizeVal
                , Css.width sizeVal
                , Css.height sizeVal
                , rowCXY
                ]

        liTextButton =
            styled button
                [ btnReset
                , hs
                , fa
                , fBody
                ]

        listItem =
            styled div [ fa, rowCY, pRm 0.5 ]

        viewContextsItem =
            let
                isSelected =
                    isCurrentPageContextList model
            in
            listItem
                [ css
                    [ boolCss isSelected [ bc <| hsla 210 1 0.56 0.3, fwb ]
                    ]
                ]
                [ liTextButton [ css [ fwb ], onClick <| SetPage ContextList ] [ text "Contexts" ]
                , Btn.iconMsg Icon.folderPlus startAddingContextMsg
                ]

        viewKeyedContextItem style vm =
            ( vm.key, viewContextItem style vm )

        viewContextItem style { name, navigateToTodoList, activeTodoCount, isSelected } =
            styled listItem
                [ style ]
                [ css
                    [ boolCss isSelected [ bc <| hsla 210 1 0.56 0.3, fwb ]
                    ]
                ]
                [ liTextButton
                    [ css
                        [ ttu
                        , rowCY
                        ]
                    , onClick navigateToTodoList
                    ]
                    [ text <| name
                    , div
                        [ css
                            [ Css.fontSize (em 0.8)
                            , Css.alignSelf Css.flexEnd
                            , Css.fontWeight Css.bold
                            , Css.property "color" "gray"
                            ]
                        ]
                        [ text <| String.fromInt activeTodoCount ]
                    ]
                ]
    in
    div
        [ class "min-h-100 bg-black-05" ]
        [ viewContextItem (Css.batch []) <| createInboxContextItemViewModel model
        , viewContextsItem
        , node "div" [] <|
            List.map (viewKeyedContextItem <| Css.batch [ plRm 1 ]) (createUserDefinedContextItemViewModel model)
        ]



-- ContextList Page


viewContextList : Model -> Html Msg
viewContextList model =
    let
        inboxViewModel : ContextViewModel Msg
        inboxViewModel =
            ContextViewModel "Inbox" "Inbox" False NoOp (SwitchToContextTodoListWithContextId ContextStore.defaultId)

        viewPrimaryListKeyed =
            getUserDefinedContextList model
                |> List.map (createContextViewModel >> viewContext)
                |> (::) (viewContext inboxViewModel)
    in
    HKeyed.node "div" [] viewPrimaryListKeyed


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
    , UI.row "pa3 w-100  bb b--light-gray"
        []
        [ txtA [ style "width" "24px" ] ""
        , button
            [ class "flex-auto pa0 ma0 tl ttu color-inherit normal underline pointer"
            , onClick switchToContextTodoList
            ]
            [ text <| "@" ++ name ]
        , boolHtml isNameEditable <| UI.fBtn Icon.edit3 startEditingName
        ]
    )



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

        --        , UI.fBtn Icon.plus startAddingTodoMsg
        ]


viewTodoList : Model -> Html Msg
viewTodoList model =
    div [ css [] ]
        [ getSelectedContextTodoList model
            |> List.map (viewKeyedTodo << createTodoViewModel model.contextStore)
            |> HKeyed.node "div" [ css [ vs ] ]
        , div [ css [ rowCY, vs ], class "ph3" ]
            [ Btn.flat [ css [ pl0 ], onClick startAddingTodoMsg ]
                [ Icons.withStyleAndAttr [] [] Icons.plus
                , div [] [ text "Add Task" ]
                ]
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
                fBtnSA [ sClass "green" ] Icon.checkCircle unmarkDone

            else
                fBtnSA [ sClass "gray" ] Icon.circle markDone
    in
    ( key
    , UI.row "pa3 bb b--light-gray"
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
        { view = Html.toUnstyled << view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
