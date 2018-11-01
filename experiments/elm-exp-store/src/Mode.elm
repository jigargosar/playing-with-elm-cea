module Mode exposing
    ( Mode
    , Msg
    , OutMsg(..)
    , init
    , startAddingContext
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
    | StartEditingTodo Todo
    | StartAddingTodo
    | StartAddingContext
    | TextInputChanged TodoContent
    | EndEditMode
    | FocusDomId DomId
    | AutoFocus


startAddingTodo =
    StartAddingTodo


startAddingContext =
    StartAddingContext


startEditing =
    StartEditingTodo


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
                EditTodoMode id _ ->
                    pure3 model

                AddTodoMode content ->
                    pure3 model

                AddContextMode name ->
                    pure3 model

                Default ->
                    EditTodoMode todo.id todo.content
                        |> pure3
                        |> andThenUpdate AutoFocus

        StartAddingTodo ->
            case model of
                EditTodoMode _ _ ->
                    pure3 model

                AddTodoMode content ->
                    pure3 model

                AddContextMode name ->
                    pure3 model

                Default ->
                    AddTodoMode ""
                        |> pure3
                        |> andThenUpdate AutoFocus

        StartAddingContext ->
            case model of
                EditTodoMode id _ ->
                    pure3 model

                AddTodoMode content ->
                    pure3 model

                AddContextMode name ->
                    pure3 model

                Default ->
                    AddContextMode ""
                        |> pure3
                        |> andThenUpdate AutoFocus

        TextInputChanged newValue ->
            case model of
                EditTodoMode id _ ->
                    EditTodoMode id newValue
                        |> pure3
                        |> addOutMsg3 (TodoContentUpdatedOutMsg id newValue)

                AddTodoMode _ ->
                    pure3 <| AddTodoMode newValue

                AddContextMode _ ->
                    pure3 <| AddContextMode newValue

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
        EditTodoMode id content ->
            viewEditContentModal modalTodoInputDomId content

        AddTodoMode content ->
            viewEditContentModal modalTodoInputDomId content

        AddContextMode name ->
            viewEditContentModal modalContextInputDomId name

        Default ->
            text ""
