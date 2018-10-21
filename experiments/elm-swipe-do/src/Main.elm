module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Style exposing (Transform(..), Unit(..))
import UI exposing (..)



---- MODEL ----


type alias Model =
    { isOpen : Bool }


init : ( Model, Cmd Msg )
init =
    ( { isOpen = False }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Toggle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Toggle ->
            ( { model | isOpen = not model.isOpen }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    UI.root
        [ viewToolbar
        , viewMagicMenu model.isOpen
            [ FeatherIcons.facebook
            , FeatherIcons.home
            , FeatherIcons.twitter
            , FeatherIcons.scissors
            , FeatherIcons.edit
            , FeatherIcons.trash2
            , FeatherIcons.filePlus
            ]
        ]


viewMagicMenu isOpen icons =
    div [ class "flex justify-center" ]
        [ div [ class "absolute bottom-1 flex flex-column items-center" ]
            ([ div [ class "bg-white z-1" ]
                [ fBtn (ter isOpen FeatherIcons.x FeatherIcons.menu) Toggle
                ]
             ]
                ++ viewMenuItems isOpen icons
            )
        ]


viewMenuItems isOpen icons =
    let
        ct =
            List.length icons |> toFloat

        transformForIdx idx =
            let
                fIdx =
                    toFloat idx

                tn =
                    -0.25 + (0.5 / (ct - 1) * fIdx)
            in
            [ Rotate (Turn tn)
            , Translate Zero (Rem -3.5)
            , Rotate (Turn -tn)
            ]

        transitionDelayForIdx idx =
            (idx * 15 |> String.fromInt) ++ "ms"
    in
    icons
        |> List.indexedMap
            (\idx i ->
                button
                    [ onClick NoOp
                    , class "flex items-center justify-center absolute pa0 ma0"
                    , Style.transform (ter isOpen (transformForIdx idx) [])
                    , style "transition" ("transform 0.3s " ++ transitionDelayForIdx idx ++ " ease-in")
                    ]
                    [ i |> FeatherIcons.toHtml [] ]
            )


ter b t f =
    if b then
        t

    else
        f



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
