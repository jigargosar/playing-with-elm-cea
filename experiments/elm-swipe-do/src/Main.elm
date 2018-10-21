module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
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
            , FeatherIcons.shoppingCart
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


type Transform
    = Rotate Unit
    | Translate Unit Unit


type Unit
    = Px Float
    | Rem Float
    | Turn Float
    | Zero


transform : List Transform -> Attribute msg
transform =
    let
        stringFromUnit unit =
            case unit of
                Px val ->
                    String.fromFloat val ++ "px"

                Rem val ->
                    String.fromFloat val ++ "rem"

                Turn val ->
                    String.fromFloat val ++ "turn"

                Zero ->
                    "0"

        stringFromTransform t =
            case t of
                Rotate unit ->
                    "rotate(" ++ stringFromUnit unit ++ ")"

                Translate unitX unitY ->
                    "translate(" ++ stringFromUnit unitX ++ "," ++ stringFromUnit unitY ++ ")"
    in
    List.map stringFromTransform >> String.join " " >> style "transform"


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
            , Translate Zero (Rem -5)
            , Rotate (Turn -tn)
            ]

        transitionDelayForIdx idx =
            (idx * 10 |> String.fromInt) ++ "ms"
    in
    icons
        |> List.indexedMap
            (\idx i ->
                div
                    [ class "absolute"
                    , transform (ter isOpen (transformForIdx idx) [])
                    , style "transition" ("all 0.3s " ++ transitionDelayForIdx idx ++ " ease-in")
                    ]
                    [ fBtn i NoOp ]
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
