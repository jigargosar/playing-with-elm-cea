module Mode exposing
    ( Mode
    , Msg
    , OutMsg(..)
    , init
    , startAddingTodo
    , startEditing
    , update
    , viewModal
    )

import BasicsX exposing (..)
import ContextStore exposing (ContextName)
import HotKey
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Log
import TodoStore exposing (Todo, TodoContent, TodoId)
import UI exposing (..)
import UpdateReturn exposing (..)


type Mode
    = Default
    | EditTodoMode TodoId TodoContent
    | AddTodoMode TodoContent
    | AddContextMode ContextName


type alias Model =
    Mode


init : Mode
init =
    Default


type Msg
    = NoOp
    | BackdropClicked
    | Warn Log.Line
    | StartEditing Todo
    | StartAddingTodo
    | StartAddingContext
    | ContentChangedInView TodoContent
    | EndEditMode
    | FocusDomId DomId


startAddingTodo =
    StartAddingTodo


startAddingContext =
    StartAddingContext


startEditing =
    StartEditing


type OutMsg
    = TodoContentUpdatedOutMsg TodoId TodoContent
    | AddTodoOutMsg TodoContent
    | AddContextOutMsg ContextName


andThenUpdate msg =
    andThen3 (update msg)


update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update message model =
    case message of
        NoOp ->
            pure3 model

        Warn logLine ->
            pure3 model
                |> addCmd3 (Log.warn "Mode.elm" logLine)

        FocusDomId domId ->
            pure3 model
                |> addCmd3 (attemptDomIdFocus domId NoOp Warn)

        BackdropClicked ->
            update EndEditMode model

        StartEditing todo ->
            EditTodoMode todo.id todo.content
                |> pure3
                |> andThenUpdate (FocusDomId modalTodoInputDomId)

        StartAddingTodo ->
            AddTodoMode ""
                |> pure3
                |> andThenUpdate (FocusDomId modalTodoInputDomId)

        StartAddingContext ->
            AddContextMode ""
                |> pure3
                |> andThenUpdate (FocusDomId modalContextInputDomId)

        ContentChangedInView newContent ->
            case model of
                EditTodoMode id _ ->
                    EditTodoMode id newContent
                        |> pure3
                        |> addOutMsg3 (TodoContentUpdatedOutMsg id newContent)

                AddTodoMode _ ->
                    pure3 <| AddTodoMode newContent

                AddContextMode _ ->
                    pure3 <| AddContextMode newContent

                Default ->
                    Debug.todo "Handle this Case"

        EndEditMode ->
            case model of
                EditTodoMode id _ ->
                    pure3 Default

                AddTodoMode content ->
                    pure3 Default
                        |> addOutMsg3 (AddTodoOutMsg content)

                AddContextMode name ->
                    pure3 Default
                        |> addOutMsg3 (AddContextOutMsg name)

                Default ->
                    Debug.todo "Handle this Case"


modalTodoInputDomId =
    "modal-todo-content-input"


modalContextInputDomId =
    "modal-context-name-input"


viewEditContentModal content =
    backdrop
        [ id "edit-content-modal-backdrop"
        , onClickTargetId
            (\targetId ->
                if targetId == "edit-content-modal-backdrop" then
                    BackdropClicked

                else
                    NoOp
            )
        ]
        [ div
            [ class "bg-white br4 shadow-1 pa3 measure w-100"
            ]
            [ div [ class "w-100 flex" ]
                [ input
                    [ id modalTodoInputDomId
                    , class "flex-auto pa3"
                    , value content
                    , onInput ContentChangedInView
                    , HotKey.onKeyDown
                        (\ke ->
                            case ke of
                                ( [], "Enter" ) ->
                                    EndEditMode

                                ( [], "Escape" ) ->
                                    EndEditMode

                                _ ->
                                    NoOp
                        )
                    ]
                    []
                ]
            ]
        ]


viewModal mode =
    case mode of
        EditTodoMode id content ->
            viewEditContentModal content

        AddTodoMode content ->
            viewEditContentModal content

        AddContextMode name ->
            viewEditContentModal name

        Default ->
            text ""
