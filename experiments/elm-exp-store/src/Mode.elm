module Mode exposing
    ( Mode
    , Msg
    , OutMsg(..)
    , init
    , startAddingContext
    , startAddingTodo
    , startEditingContext
    , startEditingTodo
    , update
    , viewModal
    )

import BasicsX exposing (..)
import ContextStore exposing (Context, ContextId, ContextName)
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
    | EditContextMode ContextId ContextName
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
    | StartAddingTodo
    | StartAddingContext
    | StartEditingTodo Todo
    | StartEditingContext Context
    | TextInputChanged TodoContent
    | EndEditMode
    | FocusDomId DomId
    | AutoFocus


startAddingTodo =
    StartAddingTodo


startAddingContext =
    StartAddingContext


startEditingTodo =
    StartEditingTodo


startEditingContext =
    StartEditingContext


type OutMsg
    = AddTodoOutMsg TodoContent
    | AddContextOutMsg ContextName
    | TodoContentUpdatedOutMsg TodoId TodoContent
    | ContextNameUpdatedOutMsg ContextId ContextName


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

        AutoFocus ->
            case model of
                EditTodoMode id _ ->
                    pure3 model
                        |> andThenUpdate (FocusDomId modalTodoInputDomId)

                AddTodoMode content ->
                    pure3 model
                        |> andThenUpdate (FocusDomId modalTodoInputDomId)

                AddContextMode name ->
                    pure3 model
                        |> andThenUpdate (FocusDomId modalContextInputDomId)

                Default ->
                    pure3 model

        BackdropClicked ->
            update EndEditMode model

        StartEditingTodo todo ->
            case model of
                Default ->
                    EditTodoMode todo.id todo.content
                        |> pure3
                        |> andThenUpdate AutoFocus

                _ ->
                    pure3 model

        StartEditingContext context ->
            case model of
                Default ->
                    EditContextMode context.id context.name
                        |> pure3
                        |> andThenUpdate AutoFocus

                _ ->
                    pure3 model

        StartAddingTodo ->
            case model of
                Default ->
                    AddTodoMode ""
                        |> pure3
                        |> andThenUpdate AutoFocus

                _ ->
                    pure3 model

        StartAddingContext ->
            case model of
                Default ->
                    AddContextMode ""
                        |> pure3
                        |> andThenUpdate AutoFocus

                _ ->
                    pure3 model

        TextInputChanged newValue ->
            case model of
                AddTodoMode _ ->
                    pure3 <| AddTodoMode newValue

                AddContextMode _ ->
                    pure3 <| AddContextMode newValue

                EditTodoMode id _ ->
                    EditTodoMode id newValue
                        |> pure3
                        |> addOutMsg3 (TodoContentUpdatedOutMsg id newValue)

                EditContextMode id _ ->
                    EditContextMode id newValue
                        |> pure3
                        |> addOutMsg3 (ContextNameUpdatedOutMsg id newValue)

                Default ->
                    pure3 model
                        |> andThenUpdate (Warn [ "Invalid Msg TextInputChanged received in Default Mode" ])

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


viewEditContentModal : DomId -> String -> Html Msg
viewEditContentModal inputId content =
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
                    [ id inputId
                    , class "flex-auto pa3"
                    , value content
                    , onInput TextInputChanged
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
        AddTodoMode content ->
            viewEditContentModal modalTodoInputDomId content

        AddContextMode name ->
            viewEditContentModal modalContextInputDomId name

        EditTodoMode id content ->
            viewEditContentModal modalTodoInputDomId content

        EditContextMode id content ->
            viewEditContentModal modalContextInputDomId content

        Default ->
            text ""
