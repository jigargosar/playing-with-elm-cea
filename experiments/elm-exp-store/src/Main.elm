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
import ListFilter exposing (Filter)
import Log
import MagicMenu exposing (MagicMenu)
import Mode exposing (Mode)
import Port
import Random
import Store exposing (Id, Item, Store, resetCache)
import Task
import UI exposing (..)
import Update2
import Update3
import UpdateReturn exposing (Update3Config, andThen, foldlOutMsgList, pure, update3)
import WheelEvent exposing (WheelEvent)



---- MODEL ----


type alias Model =
    { magicMenu : MagicMenu
    , mode : Mode
    , listFilter : ListFilter.Model
    }


type alias Flags =
    { now : Millis, todos : E.Value }


init : Flags -> ( Model, Cmd Msg )
init flags =
    pure
        { magicMenu = MagicMenu.initial
        , mode = Mode.init
        , listFilter = ListFilter.init flags.now
        }


isFilterSelected filter =
    .listFilter >> ListFilter.isSelected filter



---- UPDATE ----


andThenUpdate msg =
    andThen (update msg)


type Msg
    = NoOp
    | Warn Log.Line
    | ListFilterMsg ListFilter.Msg
    | FocusDomId String
    | MagicMenuMsg MagicMenu.Msg
    | AddClicked
    | ModeMsg Mode.Msg
    | ModeOutMsg Mode.OutMsg


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

        ListFilterMsg msg ->
            Update2.lift
                .listFilter
                (\s b -> { b | listFilter = s })
                ListFilterMsg
                ListFilter.update
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

        AddClicked ->
            update (ModeMsg <| Mode.StartAdding) model

        ModeMsg msg ->
            updateMode msg model

        ModeOutMsg msg ->
            case msg of
                _ ->
                    pure model


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



---- Subscriptions


subscriptions model =
    Sub.batch
        [ MagicMenu.subscriptions model.magicMenu |> Sub.map MagicMenuMsg
        , ListFilter.subscriptions model.listFilter |> Sub.map ListFilterMsg
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
        |> (::) (MagicMenu.Action FeatherIcons.filePlus AddClicked)


view : Model -> Html Msg
view model =
    let
        onClickSetFilter =
            onClick << ListFilterMsg << ListFilter.changeFilterMsg

        filterBtn filter label =
            button
                [ onClickSetFilter filter
                , class "pv2 "
                ]
                [ txtA
                    [ class "bb bw1"
                    , classList [ ( "b--transparent", isFilterSelected filter model |> not ) ]
                    ]
                    label
                ]
    in
    UI.root
        [ viewToolbar model
        , div [ class "w-100 flex flex-column justify-center items-center vs3 pv3" ]
            [ row ""
                []
                [ button [ onClick AddClicked ] [ text "add" ]
                , filterBtn ListFilter.Future "Future"
                , filterBtn ListFilter.Active "Active"
                , filterBtn ListFilter.Completed "Completed"
                ]
            ]
        , div [ class "w-100 flex flex-column justify-center items-center" ]
            [ MagicMenu.view mockActions MagicMenuMsg model.magicMenu ]
        , Mode.viewModal model |> Html.map ModeMsg
        ]


modalTodoInputDomId =
    "modal-todo-content-input"



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
