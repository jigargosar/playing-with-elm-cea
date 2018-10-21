module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import UI exposing (..)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    UI.root
        [ viewToolbar
        , viewMagicMenu
            [ FeatherIcons.facebook
            , FeatherIcons.home
            , FeatherIcons.twitter
            , FeatherIcons.scissors
            , FeatherIcons.shoppingCart
            ]
        ]


viewMagicMenu icons =
    div [ class "flex justify-center" ]
        [ div [ class "absolute bottom-1 flex flex-column items-center" ]
            ([ div [ class "bg-white z-1" ] [ fBtn FeatherIcons.menu NoOp ] ] ++ viewMenuItems icons)
        ]


viewMenuItems icons =
    let
        ct =
            List.length icons |> toFloat

        tran idx =
            let
                fIdx =
                    toFloat idx

                tn =
                    -0.25 + (0.5 / (ct - 1) * fIdx)
            in
            [ "rotate("
            , tn |> String.fromFloat
            , "turn) "
            , "translate(0,-5rem) "
            , "rotate("
            , -tn |> String.fromFloat
            , "turn) "
            ]
                |> String.join ""
    in
    icons
        |> List.indexedMap
            (\idx i ->
                div
                    [ class "absolute"
                    , style "transform" (tran idx)
                    , style "transition" "transform 0.3s ease-in"
                    ]
                    [ fBtn i NoOp ]
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
        , subscriptions = always Sub.none
        }
