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
    , viewModal
    )

import BasicsX exposing (..)
import ContextStore exposing (Context, ContextId, ContextName)
import DomX exposing (DomId, onClickTargetId)
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
    | Warn Log.Line
    | FocusDomId DomId
    | BackdropClicked
    | StartAddingTodo
    | StartAddingContext
    | StartEditingTodo Todo
    | StartEditingTodoContext Todo
    | StartEditingContext Context
    | TextInputChanged TodoContent
    | ContextIdChanged ContextId
    | EndEditMode
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
    , addTodo : TodoContent -> msg
    , setTodoContext : TodoId -> ContextId -> msg
    , setTodoContent : TodoId -> TodoContent -> msg
    , addContext : ContextName -> msg
    , setContextName : ContextId -> ContextName -> msg
    }


type OutMsg
    = AddTodoOutMsg TodoContent
    | AddContextOutMsg ContextName
    | TodoContentUpdatedOutMsg TodoId TodoContent
    | SetTodoContextOutMsg TodoId ContextId
    | ContextNameUpdatedOutMsg ContextId ContextName


update : UpdateConfig msg -> Msg -> Model -> ( Model, Cmd msg )
update config message model =
    let
        andThenUpdate msg =
            andThen (update config msg)

        addMaybeMsg : Maybe msg -> ( Model, Cmd msg ) -> ( Model, Cmd msg )
        addMaybeMsg maybeMsg =
            unwrapMaybe identity UpdateReturn.addMsg maybeMsg

        addCmd cmd =
            UpdateReturn.addCmd cmd >> Tuple.mapSecond (Cmd.map config.toMsg)
    in
    case message of
        NoOp ->
            pure model

        Warn logLine ->
            pure model
                |> addCmd (Log.warn "Mode.elm" logLine)

        FocusDomId domId ->
            pure model
                |> addCmd (attemptFocus NoOp Warn domId)

        AutoFocus ->
            case model of
                AddTodoMode content ->
                    pure model
                        |> andThenUpdate (FocusDomId modalTodoInputDomId)

                AddContextMode name ->
                    pure model
                        |> andThenUpdate (FocusDomId modalContextInputDomId)

                EditTodoMode id _ ->
                    pure model
                        |> andThenUpdate (FocusDomId modalTodoInputDomId)

                EditTodoContextMode todoId contextId ->
                    pure model
                        |> andThenUpdate (FocusDomId modalContextIdSelectDomId)

                EditContextMode id _ ->
                    pure model
                        |> andThenUpdate (FocusDomId modalContextInputDomId)

                Default ->
                    pure model

        BackdropClicked ->
            update config EndEditMode model

        StartEditingTodo todo ->
            case model of
                Default ->
                    EditTodoMode todo.id todo.content
                        |> pure
                        |> andThenUpdate AutoFocus

                _ ->
                    pure model

        StartEditingTodoContext todo ->
            case model of
                Default ->
                    EditTodoContextMode todo.id todo.contextId
                        |> pure
                        |> andThenUpdate AutoFocus

                _ ->
                    pure model

        StartEditingContext context ->
            case model of
                Default ->
                    EditContextMode context.id context.name
                        |> pure
                        |> andThenUpdate AutoFocus

                _ ->
                    pure model

        StartAddingTodo ->
            case model of
                Default ->
                    AddTodoMode ""
                        |> pure
                        |> andThenUpdate AutoFocus

                _ ->
                    pure model

        StartAddingContext ->
            case model of
                Default ->
                    AddContextMode ""
                        |> pure
                        |> andThenUpdate AutoFocus

                _ ->
                    pure model

        TextInputChanged newValue ->
            case model of
                AddTodoMode _ ->
                    pure <| AddTodoMode newValue

                AddContextMode _ ->
                    pure <| AddContextMode newValue

                EditTodoMode id _ ->
                    EditTodoMode id newValue
                        |> pure
                        |> addMsg (config.setTodoContent id newValue)

                EditContextMode id _ ->
                    EditContextMode id newValue
                        |> pure
                        |> addMsg (config.setContextName id newValue)

                Default ->
                    pure model
                        |> andThenUpdate (Warn [ "Invalid Msg TextInputChanged received in Default Mode" ])

                EditTodoContextMode _ _ ->
                    pure model

        ContextIdChanged newContextId ->
            case model of
                EditTodoContextMode todoId _ ->
                    pure <| EditTodoContextMode todoId newContextId

                _ ->
                    pure model
                        |> andThenUpdate (Warn [ "Invalid Msg TextInputChanged received in Invalid Mode" ])

        EndEditMode ->
            case model of
                AddTodoMode content ->
                    pure Default
                        |> addMsg (config.addTodo content)

                AddContextMode name ->
                    pure Default
                        |> addMsg (config.addContext name)

                EditTodoContextMode todoId contextId ->
                    pure Default
                        |> addMsg (config.setTodoContext todoId contextId)

                _ ->
                    pure Default


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
