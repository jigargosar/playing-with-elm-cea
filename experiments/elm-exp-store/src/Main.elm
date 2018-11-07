module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import Btn
import ContextStore exposing (Context, ContextId, ContextName, ContextStore)
import Css exposing (..)
import CssAtoms exposing (fa, fgGray, pl0, ptr, ttu)
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
import Mode exposing (Mode)
import Port
import Random
import Set exposing (Set)
import Styles exposing (..)
import Svg.Attributes
import Svg.Styled exposing (svg)
import Task
import Time
import TodoStore exposing (Todo, TodoStore)
import UI exposing (..)
import Update2
import UpdateReturn exposing (..)



---- MODEL ----


type Page
    = ContextTodoList


type alias Model =
    { todoStore : TodoStore
    , contextStore : ContextStore
    , contextId : ContextId
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
        { todoStore = todoStore
        , contextStore = contextStore
        , contextId = ContextStore.defaultId
        , mode = Mode.init
        , page = ContextTodoList
        }
        |> andThenUpdate (unwrapMaybe NoOp Warn maybeTodoStoreLogLine)
        |> andThenUpdate (unwrapMaybe NoOp Warn maybeContextStoreLogLine)


isCurrentPageContextTodoListWithContextId contextId model =
    model.page == ContextTodoList && getSelectedContextId model == contextId


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


getActiveTodoListCountForContextId : ContextId -> Model -> Int
getActiveTodoListCountForContextId cid =
    getActiveTodoListForContextId cid >> List.length


getActiveTodoListForContextId : ContextId -> Model -> List Todo
getActiveTodoListForContextId cid =
    .todoStore >> TodoStore.list >> List.filter (allPass [ contextIdEq cid, isNotDone ])


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
    | SetPage Page
    | SetContextId ContextId
    | SwitchToContextTodoListWithContextId ContextId
    | SwitchToContextTodoList
    | TodoStoreMsg TodoStore.Msg
    | ContextStoreMsg ContextStore.Msg
    | ModeMsg Mode.Msg
    | ModeOutMsg Mode.OutMsg


type alias ContextItem =
    ( String, ContextId )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        Warn logMessages ->
            ( model, Log.warn "Main" logMessages )

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

                Mode.AddContextOutMsg name ->
                    if isWhitespaceOrEmptyString name then
                        pure model

                    else
                        update (ContextStoreMsg <| ContextStore.addNew name) model

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



---- Subscriptions


subscriptions model =
    Sub.batch
        []



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
        , Mode.viewModal (getAllContextsNameIdPairs model) model.mode |> Html.map ModeMsg
        ]


viewAppBar =
    appBar []
        [ section1 [ class "pa3" ]
            [ sDiv [ fwb ] [] [ text "ELM" ], sDiv [ fontWeight lighter ] [] [ text "DONE" ] ]
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
            styled listItem
                []
                []
                [ sDiv [ fa, fwb ] [] [ text "Contexts" ]
                , Btn.icon [ onClick startAddingContextMsg ] [ Icons.plus |> Icons.default ]
                ]

        viewKeyedContextItem style vm =
            ( vm.key, viewContextItem style vm )

        viewContextItem moreStyles { name, navigateToTodoList, activeTodoCount, isSelected } =
            styled listItem
                [ moreStyles, boolCss isSelected [ bc <| hsla 210 1 0.56 0.3, fwb ] ]
                [ class "hide-child" ]
                [ liTextButton
                    [ css
                        [ ttu
                        , rowCY
                        ]
                    , onClick navigateToTodoList
                    ]
                    [ div [] [ text <| name ]
                    , sDiv
                        [ plRm 0.1
                        , Css.fontSize (em 0.8)
                        , Css.alignSelf Css.flexEnd
                        , fwb
                        , fgGray
                        ]
                        []
                        [ text <| String.fromInt activeTodoCount ]
                    ]
                , Btn.sIcon [ fgGray, opacity zero ] [ class "child" ] [ Icons.moreHDef ]
                ]
    in
    div
        [ class "min-h-100 bg-black-05" ]
        [ viewContextItem (Css.batch []) <| createInboxContextItemViewModel model
        , viewContextsItem
        , node "div" [] <|
            List.map (viewKeyedContextItem <| Css.batch [ plRm 1 ]) (createUserDefinedContextItemViewModel model)
        ]



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
        [ getSelectedContextActiveTodoList model
            |> List.map (viewKeyedTodo << createTodoViewModel model.contextStore)
            |> HKeyed.node "div" [ css [ vs ] ]
        , div [ css [ rowCY, hs ], class "ph3" ]
            [ styled Btn.flatPl0
                [ fa, fzPx 14 ]
                [ onClick startAddingTodoMsg ]
                [ Icons.plusDefault
                , text "Add Task"
                ]
            , styled Btn.flatPr0
                [ fzPx 14 ]
                [ onClick NoOp ]
                [ text "Show Completed"
                , Icons.chevronLeftDefault
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
