module Main exposing (main)

import BasicsX exposing (Millis, defaultEmptyStringTo, everyXSeconds, flip, ter, unpackResult, unwrapMaybe, when)
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
import Store exposing (Id, Item, Store, resetCache)
import Task
import TodoStore exposing (TodoStore)
import UI exposing (..)
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


getTodoList =
    .todoStore >> TodoStore.list



---- UPDATE ----


andThenUpdate msg =
    andThen (update msg)


type Msg
    = NoOp
    | Warn Log.Line
    | FocusDomId String
    | TodoStoreMsg TodoStore.Msg
    | MagicMenuMsg MagicMenu.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        Warn logMessages ->
            ( model, Log.warn "Main" logMessages )

        FocusDomId domId ->
            ( model
            , Browser.Dom.focus domId
                |> Task.attempt
                    (unpackResult
                        (\_ -> Warn [ "Focus Error: #", domId, " NotFound" ])
                        (\_ -> NoOp)
                    )
            )

        TodoStoreMsg msg ->
            Update2.lift
                .todoStore
                (\s b -> { b | todoStore = s })
                TodoStoreMsg
                TodoStore.update
                msg
                model

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


mockActions =
    [ FeatherIcons.home
    , FeatherIcons.twitter
    , FeatherIcons.scissors
    , FeatherIcons.edit
    , FeatherIcons.moon
    ]
        |> List.map (\icon -> MagicMenu.Action icon NoOp)
        |> (::) (MagicMenu.Action FeatherIcons.trash2 NoOp)
        |> (::) (MagicMenu.Action FeatherIcons.filePlus NoOp)


view : Model -> Html Msg
view model =
    UI.root
        [ viewToolbar model
        , div [ class "w-100 flex flex-column justify-center items-center vs3 pv3" ]
            [ row ""
                []
                [ button [ onClick NoOp ] [ text "add" ]
                ]
            , viewTodoList model
            ]
        , div [ class "w-100 flex flex-column justify-center items-center" ]
            [ MagicMenu.view mockActions MagicMenuMsg model.magicMenu ]
        ]


modalTodoInputDomId =
    "modal-todo-content-input"


viewTodoList model =
    div [] (List.map viewTodo (getTodoList model))


viewTodo todo =
    div [] [ text todo.content ]



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
