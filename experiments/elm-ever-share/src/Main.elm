module Main exposing (main)

import BasicsX exposing (..)
import Browser
import Browser.Dom
import Browser.Events
import Dict
import FeatherIcons
import HotKey
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Json.Decode as D
import Json.Encode as E
import Log
import MagicMenu exposing (MagicMenu)
import Mode exposing (Mode)
import Port
import Random
import Svg.Attributes
import Task
import TodoStore exposing (Todo, TodoStore)
import UI exposing (fBtnSA, row, sClass, txt, txtC)
import Update2
import Update3
import UpdateReturn exposing (Update3Config, andThen, foldlOutMsgList, pure, update3)
import WheelEvent exposing (WheelEvent)



---- MODEL ----


type alias Model =
    { magicMenu : MagicMenu
    , todoStore : TodoStore
    , mode : Mode
    }


type alias Flags =
    { now : Millis, todos : E.Value }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( maybeLogLine, todoStore ) =
            TodoStore.load flags.todos
    in
    pure
        { magicMenu = MagicMenu.initial
        , todoStore = todoStore
        , mode = Mode.init
        }
        |> andThenUpdate (unwrapMaybe NoOp Warn maybeLogLine)


getCurrentTodoList =
    .todoStore
        >> TodoStore.list
        >> List.sortBy .createdAt



---- UPDATE ----


andThenUpdate msg =
    andThen (update msg)


type Msg
    = NoOp
    | Warn Log.Line
    | TodoStoreMsg TodoStore.Msg
    | MagicMenuMsg MagicMenu.Msg
    | ModeMsg Mode.Msg
    | ModeOutMsg Mode.OutMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        Warn logMessages ->
            ( model, Log.warn "Main" logMessages )

        TodoStoreMsg msg ->
            Update2.lift
                .todoStore
                (\s b -> { b | todoStore = s })
                TodoStoreMsg
                TodoStore.update
                msg
                model

        ModeMsg msg ->
            let
                updateMode : Mode.Msg -> Model -> ( Model, Cmd Msg )
                updateMode =
                    let
                        config : Update3Config Mode Mode.Msg Mode.OutMsg Model Msg
                        config =
                            { get = .mode
                            , set = \s b -> { b | mode = s }
                            , toMsg = ModeMsg
                            , update = Mode.update
                            , toOutMsg = ModeOutMsg
                            , updateOutMsg = update
                            }
                    in
                    update3 config
            in
            updateMode msg model

        ModeOutMsg msg ->
            case msg of
                Mode.TodoContentUpdatedOutMsg id newContent ->
                    update (TodoStoreMsg <| TodoStore.setContent id newContent) model

                Mode.AddTodoWithContentOutMsg content ->
                    update (TodoStoreMsg <| TodoStore.addNew content) model

        MagicMenuMsg msg ->
            Update2.lift
                .magicMenu
                (\s b -> { b | magicMenu = s })
                MagicMenuMsg
                MagicMenu.update
                msg
                model



---- Subscriptions


subscriptions model =
    Sub.batch
        [ MagicMenu.subscriptions model.magicMenu |> Sub.map MagicMenuMsg
        ]



---- VIEW ----


startAddingMsg =
    ModeMsg Mode.startAdding


mockActions =
    [ FeatherIcons.home
    , FeatherIcons.twitter
    , FeatherIcons.scissors
    , FeatherIcons.edit
    , FeatherIcons.moon
    ]
        |> List.map (\icon -> MagicMenu.Action icon NoOp)
        |> (::) (MagicMenu.Action FeatherIcons.trash2 NoOp)
        |> (::) (MagicMenu.Action FeatherIcons.filePlus startAddingMsg)


view : Model -> Html Msg
view model =
    UI.root
        [ viewToolbar model
        , div [ class "w-100 flex flex-column justify-center items-center vs3 pv3" ]
            [ row ""
                []
                [ button [ onClick startAddingMsg ] [ text "add" ]
                ]
            , viewTodoList model
            ]
        , div [ class "w-100 flex flex-column justify-center items-center" ]
            [ MagicMenu.view mockActions MagicMenuMsg model.magicMenu ]
        , Mode.viewModal model.mode |> Html.map ModeMsg
        ]


modalTodoInputDomId =
    "modal-todo-content-input"


viewTodoList : Model -> Html Msg
viewTodoList model =
    let
        viewPrimaryListKeyed =
            getCurrentTodoList model
                |> List.map (\todo -> ( todo.id, viewTodo (createTodoViewModel todo) ))
    in
    div [ class "w-100 measure-wide" ]
        [ Html.Keyed.node "div" [] viewPrimaryListKeyed
        ]


type alias TodoViewModel msg =
    { content : String
    , done : Bool
    , startEditingContent : msg
    , markDone : msg
    , unmarkDone : msg
    , startScheduling : msg
    }


createTodoViewModel : Todo -> TodoViewModel Msg
createTodoViewModel todo =
    TodoViewModel
        (defaultEmptyStringTo "<empty>" todo.content)
        todo.done
        (ModeMsg <| Mode.startEditing todo)
        (TodoStoreMsg <| TodoStore.markDone todo.id)
        (TodoStoreMsg <| TodoStore.unmarkDone todo.id)
        NoOp


viewTodo : TodoViewModel msg -> Html msg
viewTodo { content, done, startEditingContent, markDone, unmarkDone, startScheduling } =
    div
        [ class "pa3 w-100  bb b--light-gray"
        , classList [ ( "strike gray ", done ) ]
        ]
        [ row ""
            []
            [ if done then
                fBtnSA [ sClass "green" ] FeatherIcons.checkCircle unmarkDone

              else
                fBtnSA [ sClass "gray" ] FeatherIcons.circle markDone
            , div [ class "flex-grow-1 pointer", onClick startEditingContent ] [ txt content ]
            , fBtnSA [ sClass "orange" ] FeatherIcons.clock startScheduling
            ]
        ]



--- Toolbar


viewToolbar model =
    UI.toolbar
        [ txtC "b pa3" "ELM Experiment Store"
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = {- Step.asUpdateFunction -} update
        , subscriptions = subscriptions
        }
