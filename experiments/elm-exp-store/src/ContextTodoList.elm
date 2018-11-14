module ContextTodoList exposing
    ( Model
    , Msg
    , TodoListConfig
    , cycleSelectionBy
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
import Focus
import Html.Styled exposing (Html, div, styled, text)
import Html.Styled.Attributes exposing (class, classList, css, id, tabindex)
import Html.Styled.Events exposing (onClick, onDoubleClick)
import Html.Styled.Keyed as HKeyed exposing (node)
import Icons
import Layer
import Log
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
    | SendOutMsg OutMsg
    | OnFocusResult Focus.FocusResult
    | CycleSelectionBy Int


cycleSelectionBy offset =
    CycleSelectionBy offset


type OutMsg
    = TodoStoreMsg TodoStore.Msg
    | LayerMsg Layer.Msg


update : TodoListConfig -> Msg -> Model -> ( Model, Cmd Msg, Maybe OutMsg )
update config message =
    (case message of
        NoOp ->
            withNothingOutMsg

        FocusInMsg todoId ->
            unwrapMaybe identity
                (\selectedIndex -> mapModel (\model -> { model | selectedIndex = selectedIndex }))
                (getMaybeSelectedIndexOnFocusIn todoId config)
                >> withNothingOutMsg

        ToggleCompletedVisible ->
            mapModel (\model -> { model | completedVisible = not model.completedVisible })
                >> withNothingOutMsg

        SendOutMsg outMsg ->
            withJustOutMsg outMsg

        OnFocusResult r ->
            addCmd (Log.focusResult "Main.elm" r)
                >> withNothingOutMsg

        CycleSelectionBy offset ->
            andThen
                (\model ->
                    unwrapMaybe (pure model)
                        (\( selectedIndex, todo ) ->
                            ( { model | selectedIndex = selectedIndex }
                            , Focus.attempt OnFocusResult <| getTodoDomId todo
                            )
                        )
                        (cycleSelectedIndexBy offset config model)
                )
                >> withNothingOutMsg
    )
        << pure


getSelectedContextTodoList : TodoListConfig -> List Todo
getSelectedContextTodoList { todoStore, selectedContextId } =
    todoStore |> TodoStore.listForContextId selectedContextId


getComputedSelectedIndex config model =
    let
        ( active, completed ) =
            getSelectedContextTodoList config
                |> List.partition TodoStore.isNotDone

        total =
            List.length active
    in
    min (total - 1) model.selectedIndex


getMaybeSelectedTodo config model =
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
        active |> Array.fromList |> Array.get (getComputedSelectedIndex config model)


cycleSelectedIndexBy : Int -> TodoListConfig -> Model -> Maybe ( SelectedIndex, Todo )
cycleSelectedIndexBy num config model =
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
                safeModBy total (model.selectedIndex + num)
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


type alias TodoListConfig =
    { todoStore : TodoStore
    , contextStore : ContextStore
    , selectedContextId : ContextId
    }


view : TodoListConfig -> Model -> Html Msg
view config model =
    div [ class "bl br b--black-05 flex-auto  overflow-y-scroll  pv3 flex flex-column vs3" ]
        [ viewTodoListHeader config
        , viewTodoList config model
        ]


viewTodoListHeader : TodoListConfig -> Html Msg
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


viewTodoList : TodoListConfig -> Model -> Html Msg
viewTodoList config model =
    let
        ( active, completed ) =
            TodoStore.listForContextId config.selectedContextId config.todoStore
                |> List.partition TodoStore.isNotDone
    in
    div [ css [] ]
        [ active
            |> List.indexedMap
                (\idx todo ->
                    viewKeyedTodo config (getComputedSelectedIndex config model == idx) todo
                )
            |> HKeyed.node "div" [ css [ vs ] ]
        , div [ css [ rowCY ], class "pa3" ]
            [ styled Btn.flatPl0
                [ fontSize (rem 0.8), fa ]
                [ onClick <| SendOutMsg <| LayerMsg <| Layer.OpenCreateTodoDialog config.selectedContextId ]
                [ Icons.plusSmall
                , text "Add Task"
                ]
            , viewCompletedBtn model.completedVisible
            ]
        , if model.completedVisible then
            viewCompletedSection config completed model

          else
            noHtml
        ]


viewCompletedSection config completed model =
    if List.isEmpty completed then
        sDiv [ fgGray ] [ class "pa3" ] [ text "No Completed Tasks" ]

    else
        completed
            |> List.indexedMap (\idx todo -> viewKeyedTodo config (getComputedSelectedIndex config model == idx) todo)
            |> HKeyed.node "div" [ css [ vs ] ]


viewCompletedBtn completedVisible =
    styled Btn.flatPr0
        [ fontSize (rem 0.8) ]
        [ onClick ToggleCompletedVisible ]
        [ sDiv [ rowCXY, hs ] [] [ text "Completed" ]
        , sDiv [ rowCXY, hs ]
            []
            [ if completedVisible then
                Icons.toggleRightDef

              else
                Icons.toggleLeftDef
            ]
        ]


viewKeyedTodo : TodoListConfig -> Bool -> Todo -> ( String, Html Msg )
viewKeyedTodo config isSelected todo =
    let
        doneIconBtn =
            if todo.done then
                Btn.sIcon [ fg "green" ]
                    [ onClick <|
                        SendOutMsg <|
                            TodoStoreMsg <|
                                TodoStore.unmarkDone todo.id
                    ]
                    [ Icons.checkCircle |> Icons.default ]

            else
                Btn.sIcon [ fg "gray" ]
                    [ onClick <|
                        SendOutMsg <|
                            TodoStoreMsg <|
                                TodoStore.markDone todo.id
                    ]
                    [ Icons.circle |> Icons.default ]

        domId =
            getTodoDomId todo
    in
    ( domId
    , sDiv
        [ rowCY
        , if isSelected then
            bg "lightblue"

          else
            bg "transparent"
        ]
        [ id domId, class "pa3 bb b--light-gray", tabindex 0, DomX.onFocusIn <| FocusInMsg todo.id ]
        [ sDiv [ hs, rowCY ] [] [ doneIconBtn ]
        , sDiv [ hs ]
            [ class "flex-auto flex flex-column " ]
            [ div
                [ class "pointer"
                , classList [ ( "strike gray ", todo.done ) ]
                , onDoubleClick <| SendOutMsg <| LayerMsg <| Layer.OpenEditTodoDialog todo.id
                ]
                [ sDiv [ Css.property "word-break" "break-word" ] [] [ text todo.content ] ]
            ]
        ]
    )
