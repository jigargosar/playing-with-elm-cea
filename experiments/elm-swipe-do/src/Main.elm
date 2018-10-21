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
import TodoCollection exposing (TodoCollection)
import UI exposing (..)
import WheelEvent exposing (WheelEvent)



---- MODEL ----


type alias Model =
    { magicMenu : MagicMenu, todoC : TodoCollection }


type alias Mills =
    Int


type alias Flags =
    { now : Mills }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        encTC : E.Value
        encTC =
            E.dict identity Todo.encode Dict.empty

        todoC =
            Random.step (TodoCollection.generator encTC) (Random.initialSeed flags.now)
                |> Tuple.first
    in
    ( { magicMenu = MagicMenu.initial
      , todoC = todoC
      }
    , Cmd.none
    )


setMagicMenu : MagicMenu -> Model -> Model
setMagicMenu magicMenu model =
    { model | magicMenu = magicMenu }


setTodoC : TodoCollection -> Model -> Model
setTodoC todoC model =
    { model | todoC = todoC }



---- UPDATE ----


type Msg
    = NoOp
    | MagicMenuMsg MagicMenu.Msg
    | TodoCMsg TodoCollection.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        MagicMenuMsg msg ->
            MagicMenu.update msg model.magicMenu
                |> Tuple.mapBoth (flip setMagicMenu model) (Cmd.map MagicMenuMsg)

        TodoCMsg msg ->
            TodoCollection.update msg model.todoC
                |> Tuple.mapBoth (flip setTodoC model) (Cmd.map TodoCMsg)



---- Subscriptions


subscriptions model =
    Sub.batch
        [ MagicMenu.subscriptions model.magicMenu |> Sub.map MagicMenuMsg
        ]



---- VIEW ----


mockActions =
    [ FeatherIcons.facebook
    , FeatherIcons.home
    , FeatherIcons.twitter
    , FeatherIcons.scissors
    , FeatherIcons.edit
    , FeatherIcons.trash2
    ]
        |> List.map (\icon -> MagicMenu.Action icon NoOp)
        |> (::) (MagicMenu.Action FeatherIcons.filePlus (TodoCMsg TodoCollection.NewClicked))


view : Model -> Html Msg
view model =
    UI.root
        [ viewToolbar
        , div [ class "w-100 flex flex-column justify-center items-center vs3 pv3" ]
            [ TodoCollection.viewTodoList model.todoC ]
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
