module Main exposing (main)

import Browser
import Browser.Events
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Round
import Set
import String exposing (String)
import Svg
import Svg.Attributes as SA



---- MODEL ----


type alias Flags =
    {}


type alias Model =
    { x : Int }


initialModel =
    { x = 100 }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


worldWidth =
    500


worldHeight =
    round (worldWidth * 2 / 3)



---- UPDATE ----


type Msg
    = NoOp
    | AFrame Float
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            ( m, Cmd.none )

        Reset ->
            ( initialModel, Cmd.none )

        AFrame delta ->
            let
                xDelta =
                    1
            in
            ( { m | x = m.x + xDelta }, Cmd.none )



---- VIEW ----


svgView { x } =
    Svg.svg [ HA.width worldWidth, HA.height worldHeight ]
        [ Svg.rect [ SA.width "100%", SA.height "100%", SA.fill "#adbeeb" ] []
        , Svg.circle
            [ x |> String.fromInt >> SA.cx
            , SA.cy "100"
            , SA.r "10"
            , SA.fill "#cd37a9"
            ]
            []
        ]


viewControls _ =
    H.div [] [ H.button [ HE.onClick Reset, HA.autofocus True ] [ H.text "Reset" ] ]


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "pa3 vs3" ]
            [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
            , viewControls model
            , H.div [ HA.class "" ] [ svgView model ]
            ]
        ]



---- Subscriptions ----


subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AFrame
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
