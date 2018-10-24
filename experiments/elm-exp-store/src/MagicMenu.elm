module MagicMenu exposing (Action, Actions, MagicMenu, Msg, initial, subscriptions, update, view)

import BasicsX exposing (recoverErr, ter)
import FeatherIcons
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Port
import Step exposing (Step)
import Style exposing (Transform(..), Unit(..))
import UI exposing (boolHtml, fBtn)
import WheelEvent


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
    | Clicked
    | Wheel E.Value


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Port.wheel Wheel ]


setVisibilityFromWheelEventIn model { deltaY } =
    { model | hidden = deltaY > 0 }


update : Msg -> Model -> Step Model Msg a
update message model =
    case message of
        NoOp ->
            Step.to model

        Clicked ->
            Step.to { model | open = not model.open }

        Wheel encoded ->
            D.decodeValue WheelEvent.decoder encoded
                |> Result.map (setVisibilityFromWheelEventIn model)
                |> Result.mapError (Port.logS << D.errorToString)
                |> (\result ->
                        case result of
                            Ok newModel ->
                                Step.to newModel

                            Err cmd ->
                                Step.to model |> Step.withCmd cmd
                   )



--          |> (\result ->
--                        case result of
--                            Ok { deltaY } ->
--                                { model | hidden = deltaY > 0 } |> Step.to
--
--                            Err error ->
--                                Step.to model |> Step.withCmd (Port.logS <| D.errorToString error)
--                   )
--                |> Result.map (\{ deltaY } -> { model | hidden = deltaY > 0 } |> Step.to)
--                |> recoverErr (\err -> Step.to model |> Step.withCmd (Port.logS <| D.errorToString err))


type alias Action msg =
    { icon : FeatherIcons.Icon, msg : msg }


type alias Actions msg =
    List (Action msg)


view : Actions msg -> (Msg -> msg) -> Model -> Html msg
view actions toMsg model =
    boolHtml (not model.hidden || model.open) (viewHelp actions toMsg model)


viewHelp : Actions msg -> (Msg -> msg) -> Model -> Html msg
viewHelp actions toMsg model =
    div [ class "flex justify-center" ]
        [ div [ class "absolute bottom-1 flex flex-column items-center" ]
            ([ div [ class "bg-white z-1" ]
                [ fBtn (ter model.open FeatherIcons.x FeatherIcons.menu) (toMsg Clicked)
                ]
             ]
                ++ viewMenuItems model.open actions
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
