module MagicMenu exposing (Action, Actions, MagicMenu, Msg, initial, subscriptions, update, view)

import BasicsX exposing (flip, ter, unpackResult, unwrapDecodeResult)
import FeatherIcons
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Log
import Port
import StyleOld exposing (Transform(..), Unit(..))
import Tuple exposing (pair)
import UI exposing (boolHtml, fBtn)
import Update2
import UpdateReturn exposing (pure)
import WheelEvent exposing (WheelEvent)


type alias MagicMenu =
    { open : Bool
    , hidden : Bool
    }


type alias Model =
    MagicMenu


initial =
    MagicMenu False False


type Msg
    = NoOp
    | Warn Log.Line
    | WheelEvent E.Value
    | ToggleOpen
    | Clicked
    | UpdateVisibilityFromWheelEvent WheelEvent


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Port.wheel WheelEvent ]


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            pure model

        Warn logMessages ->
            ( model, Log.warn "MagicMenu" logMessages )

        ToggleOpen ->
            pure { model | open = not model.open }

        Clicked ->
            update ToggleOpen model

        WheelEvent encoded ->
            pure model
                |> Update2.eval
                    (D.decodeValue WheelEvent.decoder encoded
                        |> unwrapDecodeResult Warn UpdateVisibilityFromWheelEvent
                        |> update
                    )

        UpdateVisibilityFromWheelEvent { deltaY } ->
            pure { model | hidden = deltaY > 0 }


type alias Action msg =
    { icon : FeatherIcons.Icon, msg : msg }


type alias Actions msg =
    List (Action msg)


view : Actions msg -> (Msg -> msg) -> Model -> Html msg
view actions toMsg model =
    boolHtml (not model.hidden || model.open) (viewHelp actions toMsg model)


viewHelp : Actions msg -> (Msg -> msg) -> Model -> Html msg
viewHelp actions toMsg model =
    div [ class " fixed  bottom-2", style "left" "50%" ]
        (viewMenuItems model.open actions
            ++ [ div [ class "absolute bg-white" ]
                    [ fBtn (ter model.open FeatherIcons.x FeatherIcons.menu) (toMsg Clicked)
                    ]
               ]
        )


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
                    , class "absolute  flex items-center justify-center pa0 ma0"
                    , StyleOld.transform (ter isOpen (transformForIdx idx) []) |> Html.Styled.Attributes.fromUnstyled
                    , style "transition" ("transform 0.3s " ++ transitionDelayForIdx idx ++ " ease-in")
                    ]
                    [ icon |> FeatherIcons.toHtml [] |> Html.fromUnstyled ]
            )
