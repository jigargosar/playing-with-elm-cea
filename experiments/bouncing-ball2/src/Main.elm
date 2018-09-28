module Main exposing (main)

import Browser
import Browser.Events
import Element exposing (el, html, layout, padding, paddingXY, rgba, row, spacing, text)
import Element.Background
import Element.Border
import Element.Font exposing (typeface)
import Element.Input
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Round
import Set
import String exposing (String)
import Svg
import Svg.Attributes as SA
import Time
import TypedSvg as TS
import TypedSvg.Attributes as TA
import TypedSvg.Types as TT
import Vec
import ViewSvg



---- MODEL ----


type alias Flags =
    {}


type alias Model =
    { pos : Vec.Vec, paused : Bool }


initialModel =
    { pos = Vec.vec 0 0, paused = False }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


worldWidth =
    500


worldHeight =
    round (worldWidth * 2 / 3)


worldSizeVec =
    Vec.fromInt worldWidth worldHeight


ballRadius =
    30



---- UPDATE ----


type Msg
    = NoOp
    | AFrame Float
    | Reset
    | TogglePause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NoOp ->
            pure m

        Reset ->
            pure initialModel

        AFrame delta ->
            let
                vel =
                    Vec.vec 1.5 0

                ret =
                    pure { m | pos = Vec.add m.pos vel }
            in
            ter m.paused (pure m) ret

        TogglePause ->
            pure { m | paused = not m.paused }


pure m =
    ( m, Cmd.none )



---- VIEW ----


viewSvg { pos } =
    Svg.svg [ HA.width worldWidth, HA.height worldHeight ]
        (ViewSvg.svgView { ballPos = pos, ballRadius = ballRadius, worldSize = worldSizeVec })


ter bool v1 v2 =
    if bool then
        v1

    else
        v2


hel ela n na ne =
    el ela (n na ne |> html)


globalStyle =
    [ Element.Font.size 16
    , Element.Font.family
        [ typeface "-apple-system"
        , typeface "BlinkMacSystemFont"
        , typeface "avenir next"
        , typeface "avenir"
        , typeface "helvetica neue"
        , typeface "helvetica"
        , typeface "ubuntu"
        , typeface "roboto"
        , typeface "noto"
        , typeface "segoe ui"
        , typeface "arial"
        , typeface "sans-serif"
        ]
    , Element.Font.family []
    ]


btn al msg labelText =
    Element.Input.button
        ([ paddingXY 6 2
         , Element.Border.rounded 0
         , Element.Border.width 0
         , Element.Border.solid
         , Element.Border.shadow
            { offset = Tuple.pair 0 0
            , size = 2
            , blur = 4
            , color = rgba 0 0 0 0.2
            }
         , Element.Border.color (rgba 0 0 0 0.1)
         , Element.Background.color (rgba 0 0 0 0)

         --         , Element.Background.gradient { angle = 0, steps = [ rgba 0 0 0 0.01, rgba 0 0 0 0.15 ] }
         ]
            ++ al
        )
        { onPress = Just msg, label = el [] (text labelText) }


viewControls { paused } =
    layout globalStyle
        (row [ spacing 8 ]
            [ btn [] Reset "Reset"
            , hel [] H.button [ HE.onClick TogglePause ] [ H.text (ter paused "Play" "Pause") ]
            ]
        )


view : Model -> Html Msg
view model =
    H.div []
        [ H.div [ HA.class "pa3 vs3" ]
            [ H.div [ HA.class "f1" ] [ H.text "Svg Animation" ]
            , viewControls model
            , H.div [ HA.class "" ] [ viewSvg model ]
            ]
        ]



---- Subscriptions ----


subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta AFrame
        , Time.every (5 * 1000) (\_ -> Reset)
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
