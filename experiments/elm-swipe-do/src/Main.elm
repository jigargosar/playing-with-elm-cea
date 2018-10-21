port module Main exposing (Model, Msg(..), init, main, update, view, wheel)

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



--- Basics


ter b t f =
    if b then
        t

    else
        f



---- Port


port wheel : (E.Value -> msg) -> Sub msg



---- MODEL ----


overMagicMenu : (MagicMenu -> MagicMenu) -> Model -> Model
overMagicMenu updateFn model =
    { model | magicMenu = updateFn model.magicMenu }


type alias Model =
    { isOpen : Bool, hideMenu : Bool, magicMenu : MagicMenu }


init : ( Model, Cmd Msg )
init =
    ( { isOpen = False, hideMenu = False, magicMenu = MagicMenu.initial }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Toggle
    | Wheel E.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Toggle ->
            ( { model | isOpen = not model.isOpen }, Cmd.none )

        Wheel ev ->
            ( D.decodeValue WheelEvent.decoder ev
                |> Result.map (updateOnWheelEvent model)
                |> Result.withDefault model
            , Cmd.none
            )


updateOnWheelEvent : Model -> WheelEvent -> Model
updateOnWheelEvent model { deltaY } =
    { model | hideMenu = deltaY > 0 }



---- Subscriptions


subscriptions model =
    Sub.batch [ wheel Wheel ]



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
        |> List.map (\icon -> Action icon NoOp)


view : Model -> Html Msg
view model =
    UI.root
        [ viewToolbar
        , boolHtml (not model.hideMenu) (viewMagicMenu model.isOpen Toggle mockActions)
        ]



--- Magic Menu View


viewMagicMenu : Bool -> msg -> Actions msg -> Html msg
viewMagicMenu isOpen menuClickMsg actions =
    div [ class "flex justify-center" ]
        [ div [ class "absolute bottom-1 flex flex-column items-center" ]
            ([ div [ class "bg-white z-1" ]
                [ fBtn (ter isOpen FeatherIcons.x FeatherIcons.menu) menuClickMsg
                ]
             ]
                ++ viewMenuItems isOpen actions
            )
        ]


viewMenuItems isOpen actions =
    let
        ct =
            List.length actions |> toFloat

        transformForIdx idx =
            let
                fIdx =
                    toFloat idx

                tn =
                    -0.25 + (0.5 / (ct - 1) * fIdx)
            in
            [ Rotate (Turn tn)
            , TranslateY (Rem -3.5)
            , Rotate (Turn -tn)
            ]

        transitionDelayForIdx idx =
            (idx * 15 |> String.fromInt) ++ "ms"
    in
    actions
        |> List.indexedMap
            (\idx { icon, msg } ->
                button
                    [ onClick msg
                    , class "flex items-center justify-center absolute pa0 ma0"
                    , Style.transform (ter isOpen (transformForIdx idx) [])
                    , style "transition" ("transform 0.3s " ++ transitionDelayForIdx idx ++ " ease-in")
                    ]
                    [ icon |> FeatherIcons.toHtml [] ]
            )



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
