module Main exposing (main)

import BasicX exposing (flip, ter)
import Browser
import Browser.Events
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import MagicMenu exposing (MagicMenu)
import Port
import Style exposing (Transform(..), Unit(..))
import UI exposing (..)
import WheelEvent exposing (WheelEvent)



---- MODEL ----


overMagicMenu : (MagicMenu -> MagicMenu) -> Model -> Model
overMagicMenu updateFn model =
    { model | magicMenu = updateFn model.magicMenu }


type alias Model =
    { magicMenu : MagicMenu }


init : ( Model, Cmd Msg )
init =
    ( { magicMenu = MagicMenu.initial }, Cmd.none )


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


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
