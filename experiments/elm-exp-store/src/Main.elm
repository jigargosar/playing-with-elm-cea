module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import Btn
import ContextPopup exposing (contextMoreMenuPopperDomId, contextMoreMenuRefDomId)
import ContextStore exposing (Context, ContextId, ContextName, ContextStore)
import Css exposing (..)
import CssAtoms exposing (fa, fgGray, p0, pl0, ptr, ttu, w100)
import Dict exposing (Dict)
import FeatherIcons as Icon
import HotKey
import Html.Styled as Html exposing (Attribute, Html, button, div, fromUnstyled, span, styled, text)
import Html.Styled.Attributes exposing (class, classList, css, id, style)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed as HKeyed exposing (node)
import Icons
import JsonCodecX exposing (Value)
import Log
import Mode exposing (Mode)
import PopupMenu
import Port
import Random
import Set exposing (Set)
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


type Popup
    = NoPopup
    | ContextPopup ContextPopup.Model


type alias Model =
    { todoStore : TodoStore
    , contextStore : ContextStore
    , contextId : ContextId
    , popup : Popup
    , mode : Mode
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
            , popup = NoPopup
            , mode = Mode.init
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


isMoreMenuOpenForContextId cid model =
    case model.popup of
        ContextPopup state ->
            ContextPopup.isOpenForContextId cid state

        _ ->
            False



---- UPDATE ----


andThenUpdate msg =
    andThen (update msg)


type Msg
    = NoOp
    | Warn Log.Line
    | SetContextId ContextId
    | SwitchToContextTodoListWithContextId ContextId
    | TodoStoreMsg TodoStore.Msg
    | ContextStoreMsg ContextStore.Msg
    | ModeMsg Mode.Msg
    | StartEditingContext ContextId
    | ContextMoreClicked ContextId
    | UpdateContextPopup ContextPopup.Msg


type alias ContextItem =
    ( String, ContextId )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        Warn logMessages ->
            ( model, Log.warn "Main" logMessages )

        SetContextId contextId ->
            pure { model | contextId = contextId }

        SwitchToContextTodoListWithContextId contextId ->
            pure model
                |> andThenUpdate (SetContextId contextId)

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
                modeUpdateConfig : Mode.UpdateConfig Msg
                modeUpdateConfig =
                    { toMsg = ModeMsg
                    , addTodo = \content -> TodoStoreMsg <| TodoStore.addNew content
                    , setTodoContext = \todoId contextId -> TodoStoreMsg <| TodoStore.setContextId todoId contextId
                    , setTodoContent = \id content -> TodoStoreMsg <| TodoStore.setContent id content
                    , addContext = ContextStoreMsg << ContextStore.addNew
                    , setContextName = \id name -> ContextStoreMsg <| ContextStore.setName id name
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
                        |> unwrapMaybe identity (andThenUpdate << ModeMsg << Mode.startEditingContext)
                   )

        ContextMoreClicked cid ->
            pure
                { model
                    | popup =
                        ContextPopup (ContextPopup.init cid)
                }
                |> andThenUpdate (UpdateContextPopup PopupMenu.popOpen)

        UpdateContextPopup msg ->
            case model.popup of
                ContextPopup cpModel ->
                    let
                        selected cid action =
                            case action of
                                ContextPopup.Rename ->
                                    StartEditingContext cid

                                ContextPopup.Delete ->
                                    ContextStoreMsg <| ContextStore.delete cid

                        config =
                            { toMsg = UpdateContextPopup
                            , selected = selected
                            }
                    in
                    ContextPopup.update config msg cpModel
                        |> mapModel (\s -> { model | popup = ContextPopup s })

                _ ->
                    pure model



---- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.popup of
            ContextPopup state ->
                ContextPopup.subscriptions state
                    |> Sub.map UpdateContextPopup

            NoPopup ->
                Sub.none
        ]



---- VIEW ----


startAddingTodoMsg =
    ModeMsg Mode.startAddingTodo


startAddingContextMsg =
    ModeMsg Mode.startAddingContext


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
        , viewPopup model
        , Mode.viewModal (getAllContextsNameIdPairs model) model.mode |> Html.map ModeMsg
        ]


viewPopup model =
    case model.popup of
        ContextPopup state ->
            ContextPopup.view { toMsg = UpdateContextPopup, state = state }

        NoPopup ->
            noHtml


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
            [ viewSidebar model
            ]
        , div [ id "popper-container", class "flex-auto  overflow-y-scroll  pv3 flex flex-column vs3" ]
            [ viewPageContent
            ]
        ]


type alias ContextItemViewModel msg =
    { key : String
    , id : ContextId
    , cid : ContextId
    , name : ContextName
    , navigateToTodoList : msg
    , activeTodoCount : Int
    , isSelected : Bool
    , moreClicked : msg
    , moreOpen : Bool
    }


createUserDefinedContextItemViewModel : Model -> List (ContextItemViewModel Msg)
createUserDefinedContextItemViewModel model =
    getUserDefinedContextList model
        |> List.map
            (\c ->
                let
                    id =
                        c.id

                    name =
                        c.name
                in
                { key = id
                , id = id
                , cid = id
                , name = name
                , navigateToTodoList =
                    SwitchToContextTodoListWithContextId id
                , activeTodoCount = getActiveTodoListCountForContextId id model
                , isSelected = isCurrentPageContextTodoListWithContextId id model
                , moreClicked = ContextMoreClicked id
                , moreOpen = isMoreMenuOpenForContextId id model
                }
            )


createInboxContextItemViewModel : Model -> ContextItemViewModel Msg
createInboxContextItemViewModel model =
    let
        id =
            ContextStore.defaultId

        name =
            ContextStore.defaultName
    in
    { key = id
    , id = id
    , cid = id
    , name = name
    , navigateToTodoList = navigateToInbox
    , activeTodoCount = getActiveTodoListCountForContextId id model
    , isSelected = isCurrentPageContextTodoListWithContextId id model
    , moreClicked = ContextMoreClicked id
    , moreOpen = isMoreMenuOpenForContextId id model
    }


viewSidebar model =
    let
        viewContextsItem =
            styled listItem
                []
                []
                [ sDiv [ fa, fwb ] [] [ text "Contexts" ]
                , Btn.icon [ onClick startAddingContextMsg ] [ Icons.plus |> Icons.default ]
                ]

        viewKeyedContextItem style vm =
            ( vm.key, viewContextItem style vm )
    in
    sDiv []
        [ class "min-h-100 bg-black-05" ]
        [ node "div" [] <|
            [ viewKeyedContextItem noStyle
                (createInboxContextItemViewModel model)
            ]
        , viewContextsItem
        , node "div" [] <|
            List.map (viewKeyedContextItem <| Css.batch [ plRm 1 ]) (createUserDefinedContextItemViewModel model)
        ]


listItem =
    styled div [ fa, rowCY, pRm 0.5 ]


liTextButton =
    styled button
        [ btnReset
        , hs
        , fa
        , fBody
        ]


viewContextItem moreStyles { name, navigateToTodoList, activeTodoCount, isSelected, moreClicked, cid } =
    styled listItem
        [ moreStyles, boolCss isSelected [ bc <| hsla 210 1 0.56 0.3, fwb ] ]
        [ id <| contextMoreMenuRefDomId cid
        , class "hide-child"
        ]
        [ liTextButton
            [ css [ ttu, rowCY ]
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
        , Btn.sIcon [ fgGray, opacity zero, focus [ opacity (int 1) ] ]
            [ class "child"
            , onClick moreClicked
            ]
            [ Icons.moreHDef ]
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
        ]


viewTodoList : Model -> Html Msg
viewTodoList model =
    div [ css [] ]
        [ getSelectedContextActiveTodoList model
            |> List.map (viewKeyedTodo << createTodoViewModel model.contextStore)
            |> HKeyed.node "div" [ css [ vs ] ]
        , div [ css [ rowCY, hs ], class "ph3" ]
            [ styled Btn.flatPl0
                [ fzPx 14, fa ]
                [ onClick startAddingTodoMsg ]
                [ Icons.plusSmall
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
