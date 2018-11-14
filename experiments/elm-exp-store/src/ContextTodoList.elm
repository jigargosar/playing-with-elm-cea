module ContextTodoList exposing (TodoListConfig, view, viewCompletedBtn, viewCompletedSection, viewKeyedTodo, viewTodoList, viewTodoListHeader)

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


view config =
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
            TodoStore.filterByContextId config.selectedContextId config.todoStore
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


viewKeyedTodo : TodoListConfig msg -> Int -> Todo -> ( String, Html msg )
viewKeyedTodo config idx todo =
    let
        doneIconBtn =
            if todo.done then
                Btn.sIcon [ fg "green" ] [ onClick <| config.unmarkDone todo.id ] [ Icons.checkCircle |> Icons.default ]

            else
                Btn.sIcon [ fg "gray" ] [ onClick <| config.markDone todo.id ] [ Icons.circle |> Icons.default ]

        domId =
            "todo-list-item-" ++ todo.id
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
