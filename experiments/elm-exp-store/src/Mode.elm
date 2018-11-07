module Mode exposing
    ( Mode
    , Msg
    , OutMsg(..)
    , UpdateConfig
    , init
    , startAddingContext
    , startAddingTodo
    , startEditingContext
    , startEditingTodo
    , startEditingTodoContext
    , update
    , updateConfig
    , viewModal
    )

import BasicsX exposing (..)
import ContextStore exposing (Context, ContextId, ContextName)
import DomEvents exposing (DomId, onClickTargetId)
import HotKey
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Log
import TodoStore exposing (Todo, TodoContent, TodoId)
import UI exposing (..)
import UpdateReturn exposing (..)


type Mode
    = Default
    | EditTodoMode TodoId TodoContent
    | EditTodoContextMode TodoId ContextId
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
    | StartEditingTodoContext Todo
    | StartEditingContext Context
    | TextInputChanged TodoContent
    | ContextIdChanged ContextId
    | EndEditMode
    | FocusDomId DomId
    | AutoFocus


startAddingTodo =
    StartAddingTodo


startAddingContext =
    StartAddingContext


startEditingTodo =
    StartEditingTodo


startEditingTodoContext =
    StartEditingTodoContext


startEditingContext =
    StartEditingContext


type alias UpdateConfig msg =
    { toMsg : Msg -> msg
    , addTodo : Maybe (TodoContent -> msg)
    , setTodoContext : Maybe (TodoId -> ContextId -> msg)
    , setTodoContent : Maybe (TodoId -> TodoContent -> msg)
    , addContext : Maybe (ContextName -> msg)
    , setContextName : Maybe (ContextId -> ContextName -> msg)
    }


updateConfig : (Msg -> msg) -> UpdateConfig msg
updateConfig toMsg =
    UpdateConfig toMsg Nothing Nothing Nothing Nothing Nothing


type OutMsg
    = AddTodoOutMsg TodoContent
    | AddContextOutMsg ContextName
    | TodoContentUpdatedOutMsg TodoId TodoContent
    | SetTodoContextOutMsg TodoId ContextId
    | ContextNameUpdatedOutMsg ContextId ContextName


update : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update config message model =
    let
        andThenUpdate msg =
            andThen3 (update config msg)
    in
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
                AddTodoMode content ->
                    pure3 model
                        |> andThenUpdate (FocusDomId modalTodoInputDomId)

                AddContextMode name ->
                    pure3 model
                        |> andThenUpdate (FocusDomId modalContextInputDomId)

                EditTodoMode id _ ->
                    pure3 model
                        |> andThenUpdate (FocusDomId modalTodoInputDomId)

                EditTodoContextMode todoId contextId ->
                    pure3 model
                        |> andThenUpdate (FocusDomId modalContextIdSelectDomId)

                EditContextMode id _ ->
                    pure3 model
                        |> andThenUpdate (FocusDomId modalContextInputDomId)

                Default ->
                    pure3 model

        BackdropClicked ->
            update config EndEditMode model

        StartEditingTodo todo ->
            case model of
                Default ->
                    EditTodoMode todo.id todo.content
                        |> pure3
                        |> andThenUpdate AutoFocus

                _ ->
                    pure3 model

        StartEditingTodoContext todo ->
            case model of
                Default ->
                    EditTodoContextMode todo.id todo.contextId
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

                EditTodoContextMode _ _ ->
                    pure3 model

        ContextIdChanged newContextId ->
            case model of
                EditTodoContextMode todoId _ ->
                    pure3 <| EditTodoContextMode todoId newContextId

                _ ->
                    pure3 model
                        |> andThenUpdate (Warn [ "Invalid Msg TextInputChanged received in Invalid Mode" ])

        EndEditMode ->
            case model of
                AddTodoMode content ->
                    pure3 Default
                        |> addOutMsg3 (AddTodoOutMsg content)

                AddContextMode name ->
                    pure3 Default
                        |> addOutMsg3 (AddContextOutMsg name)

                EditTodoContextMode todoId contextId ->
                    pure3 Default
                        |> addOutMsg3 (SetTodoContextOutMsg todoId contextId)

                _ ->
                    pure3 Default


modalTodoInputDomId =
    "modal-todo-content-input"


modalContextInputDomId =
    "modal-context-name-input"


modalContextIdSelectDomId =
    "modal-context-id-select"


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


type alias EditTodoContextModalViewModel =
    { contexts : List ( String, ContextId )
    , selectedContextId : ContextId
    , todoContent : String
    }


createEditTodoContextModalViewModel contexts todoId contextId =
    EditTodoContextModalViewModel
        contexts
        contextId
        todoId


viewEditTodoContextModal : EditTodoContextModalViewModel -> Html Msg
viewEditTodoContextModal { selectedContextId, contexts } =
    let
        backDropId =
            "edit-todo-context-modal-backdrop"
    in
    backdrop
        [ id backDropId
        , onClickTargetId
            (\targetId ->
                if targetId == backDropId then
                    BackdropClicked

                else
                    NoOp
            )
        ]
        [ div
            [ class "bg-white br4 shadow-1 pa3 measure w-100"
            ]
            [ div [ class "w-100 flex" ]
                [ select
                    [ id modalContextIdSelectDomId
                    , class "flex-auto pa3"
                    , onInput ContextIdChanged
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
                    (contexts
                        |> List.map
                            (\( name, contextId ) ->
                                option [ selected (contextId == selectedContextId), value contextId ] [ text name ]
                            )
                    )
                ]
            ]
        ]


viewModal contexts mode =
    case mode of
        AddTodoMode content ->
            viewEditContentModal modalTodoInputDomId content

        AddContextMode name ->
            viewEditContentModal modalContextInputDomId name

        EditTodoMode id content ->
            viewEditContentModal modalTodoInputDomId content

        EditTodoContextMode todoId contextId ->
            viewEditTodoContextModal <| createEditTodoContextModalViewModel contexts todoId contextId

        EditContextMode id content ->
            viewEditContentModal modalContextInputDomId content

        Default ->
            text ""
