module ContextTodoList exposing
    ( Model
    , TodoListConfig
    , getTodoDomId
    , init
    , update
    , view
    , viewCompletedBtn
    , viewCompletedSection
    , viewKeyedTodo
    , viewTodoList
    , viewTodoListHeader
    )

import Array
import BasicsX exposing (..)
import Btn
import ContextStore exposing (ContextId, ContextStore)
import Css exposing (..)
import DomX
import Html.Styled exposing (Html, div, styled, text)
import Html.Styled.Attributes exposing (class, classList, css, id, tabindex)
import Html.Styled.Events exposing (onClick, onDoubleClick)
import Html.Styled.Keyed as HKeyed exposing (node)
import Icons
import Styles exposing (..)
import TodoStore exposing (Todo, TodoId, TodoStore)
import UI exposing (..)
import UpdateReturn exposing (..)


type alias SelectedIndex =
    Int


type alias Model =
    { selectedIndex : SelectedIndex
    , completedVisible : Bool
    }


init =
    { selectedIndex = 0, completedVisible = False }


getTodoDomId todo =
    "todo-list-item" ++ todo.id


type Msg
    = NoOp
    | FocusInMsg TodoId
    | ToggleCompletedVisible


type OutMsg
    = None


update : Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update message model =
    case message of
        NoOp ->
            withNothingOutMsg

        FocusInMsg todoId ->
            ( model, Cmd.none )

        ToggleCompletedVisible ->
            ( model, Cmd.none )


type alias Config =
    { todoStore : TodoStore
    , selectedContextId : ContextId
    , selectedIndex : Int
    }


getSelectedContextTodoList : Config -> List Todo
getSelectedContextTodoList { todoStore, selectedContextId } =
    todoStore |> TodoStore.listForContextId selectedContextId


getComputedSelectedIndex config =
    let
        ( active, completed ) =
            getSelectedContextTodoList config
                |> List.partition TodoStore.isNotDone

        total =
            List.length active
    in
    min (total - 1) config.selectedIndex


getMaybeSelectedTodo config =
    let
        ( active, completed ) =
            getSelectedContextTodoList config
                |> List.partition TodoStore.isNotDone

        total =
            List.length active
    in
    if total <= 0 then
        Nothing

    else
        active |> Array.fromList |> Array.get (getComputedSelectedIndex config)


cycleSelectedIndexBy : Int -> Config -> Maybe ( SelectedIndex, Todo )
cycleSelectedIndexBy num config =
    let
        ( active, completed ) =
            getSelectedContextTodoList config
                |> List.partition TodoStore.isNotDone

        total =
            List.length active
    in
    if total > 0 then
        let
            selectedIndex =
                safeModBy total (config.selectedIndex + num)
        in
        Array.fromList active
            |> Array.get selectedIndex
            |> Maybe.map (\todo -> ( selectedIndex, todo ))

    else
        Nothing


getMaybeSelectedIndexOnFocusIn todoId config =
    let
        ( active, completed ) =
            getSelectedContextTodoList config
                |> List.partition TodoStore.isNotDone
    in
    Array.fromList active
        |> Array.toIndexedList
        |> List.filter (Tuple.second >> .id >> eqs todoId)
        |> List.head
        |> Maybe.map Tuple.first


type alias TodoListConfig msg =
    { todoStore : TodoStore
    , contextStore : ContextStore
    , toggleShowCompleted : msg
    , isShowingCompleted : Bool
    , selectedIndex : Int
    , markDone : TodoId -> msg
    , unmarkDone : TodoId -> msg
    , focusInMsg : TodoId -> msg
    , editMsg : TodoId -> msg
    , selectedContextId : ContextId
    , addNewMsg : msg
    }


view config model =
    div [ class "bl br b--black-05 flex-auto  overflow-y-scroll  pv3 flex flex-column vs3" ]
        [ viewTodoListHeader config
        , viewTodoList config
        ]


viewTodoListHeader : TodoListConfig msg -> Html msg
viewTodoListHeader { selectedContextId, contextStore } =
    let
        name =
            ContextStore.getNameOrDefaultById selectedContextId contextStore
    in
    div
        [ class "ph3 flex flex-row" ]
        [ div [ class "flex-auto" ]
            [ text name ]
        ]


viewTodoList : TodoListConfig msg -> Html msg
viewTodoList config =
    let
        ( active, completed ) =
            TodoStore.listForContextId config.selectedContextId config.todoStore
                |> List.partition TodoStore.isNotDone
    in
    div [ css [] ]
        [ active
            |> List.indexedMap (\idx todo -> viewKeyedTodo config idx todo)
            |> HKeyed.node "div" [ css [ vs ] ]
        , div [ css [ rowCY ], class "pa3" ]
            [ styled Btn.flatPl0
                [ fontSize (rem 0.8), fa ]
                [ onClick config.addNewMsg ]
                [ Icons.plusSmall
                , text "Add Task"
                ]
            , viewCompletedBtn config
            ]
        , if config.isShowingCompleted then
            viewCompletedSection config completed

          else
            noHtml
        ]


viewCompletedSection config completed =
    if List.isEmpty completed then
        sDiv [ fgGray ] [ class "pa3" ] [ text "No Completed Tasks" ]

    else
        completed
            |> List.indexedMap (\idx todo -> viewKeyedTodo config idx todo)
            |> HKeyed.node "div" [ css [ vs ] ]


viewCompletedBtn { isShowingCompleted, toggleShowCompleted } =
    styled Btn.flatPr0
        [ fontSize (rem 0.8) ]
        [ onClick toggleShowCompleted ]
        [ sDiv [ rowCXY, hs ] [] [ text "Completed" ]
        , sDiv [ rowCXY, hs ]
            []
            [ if isShowingCompleted then
                Icons.toggleRightDef

              else
                Icons.toggleLeftDef
            ]
        ]


viewKeyedTodo : TodoListConfig msg -> Int -> Todo -> ( String, Html msg )
viewKeyedTodo config idx todo =
    let
        doneIconBtn =
            if todo.done then
                Btn.sIcon [ fg "green" ] [ onClick <| config.unmarkDone todo.id ] [ Icons.checkCircle |> Icons.default ]

            else
                Btn.sIcon [ fg "gray" ] [ onClick <| config.markDone todo.id ] [ Icons.circle |> Icons.default ]

        domId =
            getTodoDomId todo
    in
    ( domId
    , sDiv
        [ rowCY
        , if config.selectedIndex == idx then
            bg "lightblue"

          else
            bg "transparent"
        ]
        [ id domId, class "pa3 bb b--light-gray", tabindex 0, DomX.onFocusIn <| config.focusInMsg todo.id ]
        [ sDiv [ hs, rowCY ] [] [ doneIconBtn ]
        , sDiv [ hs ]
            [ class "flex-auto flex flex-column " ]
            [ div
                [ class "pointer"
                , classList [ ( "strike gray ", todo.done ) ]
                , onDoubleClick (config.editMsg todo.id)
                ]
                [ sDiv [ Css.property "word-break" "break-word" ] [] [ text todo.content ] ]
            ]
        ]
    )
