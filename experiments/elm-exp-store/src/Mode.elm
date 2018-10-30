module Mode exposing
    ( Mode(..)
    , Model
    , Msg(..)
    , OutMsg(..)
    , editContentMode
    , init
    , update
    , viewModal
    )

import HotKey
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Store
import Todo exposing (TodoItem)
import UI exposing (..)
import UpdateReturn exposing (..)


type Mode
    = Default
    | EditContentMode Store.Id Todo.Content
    | AddContentMode Todo.Content


type alias Model =
    Mode


init : Mode
init =
    Default


type Msg
    = NoOp
    | StartEditing TodoItem
    | StartAdding
    | ContentChangedInView Todo.Content
    | EndEditMode


editContentMode todo =
    EditContentMode todo.meta.id todo.attrs.content


type OutMsg
    = TodoInputContentChangedOutMsg Store.Id Todo.Content
    | FocusDomIdOutMsg String


update : Msg -> Model -> ( Model, Cmd Msg, List OutMsg )
update message model =
    case message of
        NoOp ->
            pure3 model

        StartEditing todo ->
            editContentMode todo
                |> pure3
                |> addOutMsg3 (FocusDomIdOutMsg modalTodoInputDomId)

        StartAdding ->
            AddContentMode ""
                |> pure3
                |> addOutMsg3 (FocusDomIdOutMsg modalTodoInputDomId)

        ContentChangedInView newContent ->
            case model of
                EditContentMode id _ ->
                    EditContentMode id newContent
                        |> pure3
                        |> addOutMsg3 (TodoInputContentChangedOutMsg id newContent)

                AddContentMode _ ->
                    pure3 <| AddContentMode newContent

                Default ->
                    Debug.todo "Handle this Case"

        EndEditMode ->
            case model of
                EditContentMode id _ ->
                    pure3 Default

                _ ->
                    Debug.todo "Handle this Case"


modalTodoInputDomId =
    "modal-todo-content-input"


viewEditContentModal todoId content =
    backdrop []
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


viewModal model =
    case model.mode of
        EditContentMode id content ->
            viewEditContentModal (Just id) content

        AddContentMode content ->
            viewEditContentModal Nothing content

        Default ->
            text ""
