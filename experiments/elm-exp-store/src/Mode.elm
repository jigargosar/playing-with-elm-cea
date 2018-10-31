module Mode exposing
    ( Mode
    , Msg
    , OutMsg(..)
    , init
    , startAdding
    , update
    , viewModal
    )

import BasicsX exposing (..)
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
    | EditContentMode TodoId TodoContent
    | AddContentMode TodoContent


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
    | StartAdding
    | ContentChangedInView TodoContent
    | EndEditMode
    | FocusDomId DomId


startAdding =
    StartAdding


type OutMsg
    = TodoContentUpdatedOutMsg TodoId TodoContent
    | AddTodoWithContentOutMsg TodoContent


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
            EditContentMode todo.id todo.content
                |> pure3
                |> andThenUpdate (FocusDomId modalTodoInputDomId)

        StartAdding ->
            AddContentMode ""
                |> pure3
                |> andThenUpdate (FocusDomId modalTodoInputDomId)

        ContentChangedInView newContent ->
            case model of
                EditContentMode id _ ->
                    EditContentMode id newContent
                        |> pure3
                        |> addOutMsg3 (TodoContentUpdatedOutMsg id newContent)

                AddContentMode _ ->
                    pure3 <| AddContentMode newContent

                Default ->
                    Debug.todo "Handle this Case"

        EndEditMode ->
            case model of
                EditContentMode id _ ->
                    pure3 Default

                AddContentMode content ->
                    pure3 Default
                        |> addOutMsg3 (AddTodoWithContentOutMsg content)

                _ ->
                    Debug.todo "Handle this Case"


modalTodoInputDomId =
    "modal-todo-content-input"


viewEditContentModal todoId content =
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
        EditContentMode id content ->
            viewEditContentModal (Just id) content

        AddContentMode content ->
            viewEditContentModal Nothing content

        Default ->
            text ""
