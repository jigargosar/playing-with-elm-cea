module Main exposing (main)

import BasicsX exposing (flip, ter)
import Browser
import Browser.Events
import Collection exposing (Collection)
import Dict
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import MagicMenu exposing (MagicMenu)
import Port
import Random
import Style exposing (Transform(..), Unit(..))
import Task
import Todo exposing (Todo)
import Todos exposing (Todos)
import UI exposing (..)
import WheelEvent exposing (WheelEvent)



---- MODEL ----


type alias Model =
    { magicMenu : MagicMenu, todos : Todos }


type alias Mills =
    Int


type alias Flags =
    { now : Mills, todos : E.Value }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        todos =
            Random.step (Todos.generator flags.todos) (Random.initialSeed flags.now)
                |> Tuple.first
    in
    ( { magicMenu = MagicMenu.initial
      , todos = todos
      }
    , Cmd.none
    )


setMagicMenu : MagicMenu -> Model -> Model
setMagicMenu magicMenu model =
    { model | magicMenu = magicMenu }


setTodoC : Todos -> Model -> Model
setTodoC todos model =
    { model | todos = todos }



---- UPDATE ----


type Msg
    = NoOp
    | MagicMenuMsg MagicMenu.Msg
    | TodoCMsg Todos.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        MagicMenuMsg msg ->
            MagicMenu.update msg model.magicMenu
                |> Tuple.mapBoth (flip setMagicMenu model) (Cmd.map MagicMenuMsg)

        TodoCMsg msg ->
            Todos.update msg model.todos
                |> Tuple.mapBoth (flip setTodoC model) (Cmd.map TodoCMsg)



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
        |> (::) (MagicMenu.Action FeatherIcons.trash2 (TodoCMsg Todos.Reset))
        |> (::) (MagicMenu.Action FeatherIcons.filePlus (TodoCMsg Todos.NewClicked))


view : Model -> Html Msg
view model =
    UI.root
        [ viewToolbar
        , Html.map TodoCMsg <|
            div [ class "w-100 flex flex-column justify-center items-center vs3 pv3" ]
                [ Todos.view model.todos ]
        , div [ class "w-100 flex flex-column justify-center items-center" ]
            [ MagicMenu.view mockActions MagicMenuMsg model.magicMenu ]
        ]



--- Toolbar


viewToolbar =
    UI.toolbar
        [ txtC "b ph3" "ELM Swipe Do"
        , spacer
        , viewTabs
            [ viewTab False "Scheduled"
            , viewTab True "Todo"
            , viewTab False "Done"
            ]
        , spacer
        ]


viewTab active l =
    UI.txtCL [ ( "b ph3 pv2", True ), ( "bw2 bb b--blue", active ) ] l


viewTabs =
    div [ class "flex bw2 bt b--transparent" ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
