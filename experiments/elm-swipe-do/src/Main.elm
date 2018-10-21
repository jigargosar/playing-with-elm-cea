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
import Todo exposing (Todo)
import UI exposing (..)
import WheelEvent exposing (WheelEvent)



---- MODEL ----


type alias TodoC =
    Collection Todo


type alias Model =
    { magicMenu : MagicMenu, todoC : TodoC }


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
    in
    ( { magicMenu = MagicMenu.initial
      , todoC =
            Random.step (Collection.generator Todo.decoder encTC) (Random.initialSeed flags.now)
                |> Tuple.first
      }
    , Cmd.none
    )


setMagicMenu : MagicMenu -> Model -> Model
setMagicMenu magicMenu model =
    { model | magicMenu = magicMenu }



---- UPDATE ----


type Msg
    = NoOp
    | MagicMenuMsg MagicMenu.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        MagicMenuMsg msg ->
            MagicMenu.update msg model.magicMenu
                |> Tuple.mapBoth (flip setMagicMenu model) (Cmd.map MagicMenuMsg)



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
    , FeatherIcons.filePlus
    ]
        |> List.map (\icon -> MagicMenu.Action icon NoOp)


view : Model -> Html Msg
view model =
    UI.root
        [ viewToolbar
        , MagicMenu.view mockActions MagicMenuMsg model.magicMenu
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
